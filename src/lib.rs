use dyn_clone::DynClone;
use std::collections::HashMap;
use thiserror::Error;
mod peloteurs;
#[macro_export]
macro_rules! sym {
    ($s:expr) => {
        Form::Symbol($s.to_string(), TypeInfo::new(0,0,TypeDecl::TString))
    };
    ($s:expr,$l:expr,$c:expr) => {
        Form::Symbol(
            $s.to_string(),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TString,
            ),
        )
    };
}
#[macro_export]
macro_rules! int {
    ($i:expr) => {
        Form::Number(Numerical::Int64($i), TypeInfo::new(0, 0, TypeDecl::TInt64))
    };
    ($i:expr,$l:expr,$c:expr) => {
        Form::Number(
            Numerical::Int64($i),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TInt64,
            ),
        )
    };
}
#[macro_export]
macro_rules! float {
    ($i:expr) => {
        Form::Number(Numerical::Float64($i), TypeInfo::new(0,0,TypeDecl::TFloat64))
    };
    ($i:expr,$l:expr,$c:expr) => {
        Form::Number(
            Numerical::Float64($i),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TFloat64,
            ),
        )
    };
}
#[macro_export]
macro_rules! bool_t {
    () => {
        Form::Atom(Atomic::Boolean(Logical::True, TypeInfo::new(0,0,TypeDecl::TBoolean)))
    };
}
#[macro_export]
macro_rules! bool_f {
    () => {
        Form::Atom(Atomic::Boolean(Logical::False, TypeInfo::new(0,0,TypeDecl::TBoolean)))
    };
}
#[macro_export]
macro_rules! list {
    () => (
        Form::List(Vec::new(),TypeInfo::default())
    );
    ($($x:expr),+ $(,)?) => (
        Form::List(vec![$($x),+],TypeInfo::default())
    );
}
#[macro_export]
macro_rules! sym_matches {
    ($h:expr, $($x:expr),+ $(,)?) => (
        vec![$($x),+].iter().fold(false, |r,c| r || $h.matches_string(c).unwrap_or(false) )
    );
}
#[macro_export]
macro_rules! internal {
    ($e:expr) => {
        LustError::Internal($e.to_string())
    };
}
#[macro_export]
macro_rules! unexpected {
    ($u:expr,$w:expr) => {
        LustError::Unexpected($w.to_string(), $u.to_string())
    };
}

#[derive(Error, Debug, PartialEq)]
pub enum LustError {
    #[error("{0}")]
    Internal(String),
    #[error("{0}")]
    Syntax(String),
    #[error("{0} cannot {1}")]
    Semantic(String, String),
    #[error("expected {0} but got {1}")]
    Unexpected(String, String),
    #[error("unknown error occurred")]
    Unknown,
}
static NON_LIST_FN: &str = "cannot called on non list";
type Result<T> = std::result::Result<T, LustError>;
#[derive(Debug, Clone, PartialEq)]
enum TypeDecl {
    TUndeclared,
    TAny,
    TInt64,
    TFloat64,
    TString,
    TSymbol,
    TBoolean,
    TLambda, // Generic lambda
    TLambdaOf(Box<TypeDecl>,Box<TypeDecl>), // ListOf types in, ListOf types out
    TList,
    TListOf(Vec<TypeDecl>),
    TUnion(Vec<TypeDecl>),
    TUser(usize), 
}
impl TypeDecl {
    fn as_boxed(&self) -> Box<TypeDecl> {
        Box::new(self.clone())
    }
    fn same_as(&self, other: &Self) -> bool {
        let l = self.to_ord();
        let r = other.to_ord();
        let count = l.iter().zip(&r).filter(|&(a,b)| a == b).count(); 
        count == l.len() && count == r.len()
    }
    fn similar_to(&self, other: &Self) -> bool {
        let mut l = self.to_ord();
        let mut r = self.to_ord();
        l.sort();
        r.sort();
        l == r
    }
    fn to_ord(&self) -> Vec<usize> { // compare types
        match self {
            Self::TUndeclared => vec![0],
            Self::TAny => vec![1],
            Self::TInt64 => vec![2],
            Self::TFloat64 => vec![3],
            Self::TString => vec![4],
            Self::TSymbol => vec![5],
            Self::TBoolean => vec![6],
            Self::TLambda => vec![7],
            Self::TLambdaOf(left,right) => {
                let mut v:Vec<usize> = vec![8];
                let left_v = left.clone().to_ord();
                let right_v = right.clone().to_ord();
                for i in &left_v {
                    v.push(*i);
                }
                for i in &right_v {
                    v.push(*i);
                }
                v
            },
            Self::TList => vec![9],
            Self::TListOf(lst) => {
                let mut v:Vec<usize> = vec![10];
                for i in lst {
                    for j in &i.to_ord() {
                        v.push(*j);
                    }
                }
                v
            },
            Self::TUnion(lst) => {
                let mut v:Vec<usize> = vec![11];
                for i in lst {
                    for j in &i.to_ord() {
                        v.push(*j);
                    }
                }
                v.sort(); // because union order doesn't matter
                v
            },
            Self::TUser(i) => vec![ i + 12],
        }
    }
}
#[derive(Clone)]
struct TypeDeclManager {
    user_defined_types: Vec<String>,
}
impl TypeDeclManager {
    fn new() -> Self {
        Self {
            user_defined_types: vec![],
        }
    }
    fn add_str(&mut self,utype: &str) -> Result<usize> {
        self.add(utype.to_string()) 
    }
    fn add(&mut self,utype: String) -> Result<usize> {
        for user_type in &self.user_defined_types {
           if *user_type == utype {
               return Err(internal!(format!("Cannot redefine type {}!",utype)))
           } 
        }
        self.user_defined_types.push(utype);
        Ok(self.user_defined_types.len() - 1)
    }
    fn parse_str(&self,tstr: &str) -> Result<TypeDecl> {
        match tstr {
            "any_t" => Ok(TypeDecl::TAny),
            "i64_t" | "i_t" => Ok(TypeDecl::TInt64),
            "f64_t" | "f_t" => Ok(TypeDecl::TFloat64),
            "str_t" | "s_t" => Ok(TypeDecl::TString),
            "bool_t"| "b_t" => Ok(TypeDecl::TBoolean),
            "list_t" | "l_t" => Ok(TypeDecl::TList),
            _ => {
                let as_str = tstr.to_string();
                for (i,user_type) in self.user_defined_types.iter().enumerate() {
                   if as_str == *user_type {
                       return Ok(TypeDecl::TUser(i)); 
                   }
                }
                Err(internal!(format!("{} is not a defined type",as_str)))
            }
        }
    }
    fn parse_form(&self,typ: Form) -> Result<TypeDecl> {
        match &typ {
            Form::List(lst,..) => {
                if typ.is_lambda()? {
                   let in_def = typ.get_param_types(self)?;
                   let out_def = typ.get_result_types(self)?;
                   Ok(TypeDecl::TLambdaOf(in_def.as_boxed(),out_def.as_boxed()))
                } else {
                    let mut types = vec![];
                    for t in lst {
                        types.push(self.parse_form(t.clone())?);
                    }
                    Ok(TypeDecl::TListOf(types))
                }
            },
            Form::Symbol(s,..) => Ok(self.parse_str(s)?),
            _ => Err(internal!(format!("cannot convert {:?} to a TypeDecl",typ))),
        }
    }
    fn is_type_keyword(&self, typ: Form) -> bool {
        if sym_matches!(typ,"any_t","i64_t","i_t","f64_t","f_t","str_t","s_t","bool_t","b_t","fn_t","list_t","l_t") {
            return true;
        }
        let as_str = typ.as_string();
        if let Ok(ok_str) = as_str {
            for (i,user_type) in self.user_defined_types.iter().enumerate() {
               if ok_str == *user_type {
                   return true; 
               }
            }
        }
        false
    }
}
#[derive(Debug, Clone)]
struct TypeInfo {
    line: usize,
    column: usize,
    decl: TypeDecl,
}
impl TypeInfo {
    fn new(l: usize, c: usize, d: TypeDecl) -> Self {
        Self { line: l, column: c, decl: d }
    }
    fn default() -> Self {
        Self::new(0,0,TypeDecl::TAny)
    }
}
#[derive(Debug, Clone)]
enum Numerical {
    Int64(i64),
    Float64(f64),
}
#[derive(Debug, Clone)]
enum Logical {
    True,
    False,
}
#[derive(Debug, Clone)]
enum Atomic {
    Symbol(Box<Form>),
    Number(Box<Form>),
    Boolean(Logical, TypeInfo),
}
#[derive(Debug, Clone)]
enum Expressable {
    Atom(Box<Form>),
    List(Box<Form>),
}
#[derive(Debug, Clone)]
enum Form {
    Symbol(String, TypeInfo),
    Number(Numerical, TypeInfo),
    /// Symbol, Number
    Atom(Atomic),
    List(Vec<Form>,TypeInfo),
    /// Atom, List
    Exp(Expressable),
}
impl Form {
    fn get_type(&self) -> TypeDecl {
        match self {
            Self::Atom(Atomic::Number(ref n)) => {
               if let Self::Number(_,info) = &**n {
                   info.decl.clone()
               } else {
                    panic!("wtf");
               }
            }, 
            Self::Number(_,info) => info.decl.clone(),
            Self::List(..) => TypeDecl::TList,
            Self::Symbol(_,info) => info.decl.clone(),
            Self::Exp(Expressable::Atom(a)) => a.get_type(),
            Self::Exp(Expressable::List(..)) => TypeDecl::TList,
            _ => panic!("wtf"),
       }
    }
    fn get_param_types(&self, manager: &TypeDeclManager) -> Result<TypeDecl> {
        match self {
            Form::List(elements,..) => {
                let mut in_types = vec![];
                let lst = elements[1].clone();
                let mut i = 1; // because 0 % 2 == 0
                for e in lst.as_vec()?.into_iter() {
                    if i % 2 == 0 {
                        in_types.push(manager.parse_form(e)?);
                    }
                    i += 1;
                }
                Ok(TypeDecl::TListOf(in_types))
            },
            _ => Err(internal!("not a lambda!")),
        }
    }
    fn get_result_types(&self, manager: &TypeDeclManager) -> Result<TypeDecl> {
        match self {
            Form::List(elements,..) => {
                let mut out_types = vec![];
                let lst = elements[2].clone();
                for e in lst.as_vec()?.into_iter() {
                    out_types.push(manager.parse_form(e)?);
                }
                Ok(TypeDecl::TListOf(out_types))
            },
            _ => Err(internal!("not a lambda!")),
        }
    }
    fn is_same_type_as(&self,other:Form) -> bool {
       let self_type = self.get_type();
       let other_type = other.get_type();
       self_type == other_type
    }
    fn is_type(&self, typ: TypeDecl) -> bool {
        self.get_type() == typ
    }
    fn is_atom(&self) -> bool {
        matches!(self, Self::Atom(..))
    }
    fn is_number(&self) -> bool {
        matches!(self, Self::Atom(Atomic::Number(..))) || matches!(self, Self::Number(..))
    }
    fn is_int(&self) -> bool {
        self.is_number() && matches!(self, Self::Number(Numerical::Int64(..), ..))
    }
    fn is_float(&self) -> bool {
        self.is_number() && matches!(self, Self::Number(Numerical::Float64(..), ..))
    }
    fn is_symbol(&self) -> bool {
        matches!(self, Self::Atom(Atomic::Symbol(..))) || matches!(self, Self::Symbol(..))
    }
    fn is_exp(&self) -> bool {
        matches!(self, Self::Exp(..))
    }
    fn is_list(&self) -> bool {
        matches!(self, Self::List(..))
    }
    fn is_lambda(&self) -> Result<bool> {
        match self {
            Self::List(l,_) => Ok(l[0].clone().matches_string("fn")?),
            _ => Ok(false),
        }
    }
    fn is_true(&self) -> Result<bool> {
        if self.is_symbol() && self.clone().as_string()? == "true" {
            return Ok(true);
        }
        Ok(matches!(self, Self::Atom(Atomic::Boolean(Logical::True,..))))
    }
    fn is_false(&self) -> Result<bool> {
        if self.is_symbol() && self.clone().as_string()? == "false" {
            return Ok(true);
        }
        Ok(matches!(self, Self::Atom(Atomic::Boolean(Logical::False,..))))
    }
    fn is_bool(&self) -> Result<bool> {
        Ok(matches!(self, Self::Atom(Atomic::Boolean(..))))
    }
    fn matches_string(&self, s: &str) -> Result<bool> {
        if !self.is_symbol() {
            return Ok(false);
        }
        if self.clone().as_string()? == s {
            return Ok(true);
        }
        Ok(false)
    }
    fn as_bool(&self) -> Result<bool> {
        if self.is_bool()? {
            if self.is_true()? {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Err(internal!("cannot call as_bool on non boolean symbol"))
        }
    }
    fn as_symbol(&self) -> Result<Form> {
        match self.clone() {
            Self::Symbol(..) => Ok(self.clone()),
            Self::Atom(Atomic::Symbol(s)) => Ok(*s),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "be reduced to a Symbol".to_string(),
            )),
        }
    }
    fn as_number(&self) -> Result<Form> {
        match self.clone() {
            Self::Atom(Atomic::Number(n)) => Ok(*n),
            Self::Number(..) => Ok(self.clone()),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "be reduced to a number".to_string(),
            )),
        }
    }
    fn as_string(&self) -> Result<String> {
        match self {
            Self::Symbol(s, _) => Ok(s.to_string()),
            _ => Err(unexpected!("Symbol", format!("{:?} to as_string", self))),
        }
    }
    fn as_int(&self) -> Result<i64> {
        match self {
            Self::Number(Numerical::Int64(i), ..) => Ok(*i),
            _ => Err(unexpected!("Number", format!("{:?} to as_int", self))),
        }
    }
    fn as_float(&self) -> Result<f64> {
        match self {
            Self::Number(Numerical::Float64(f), ..) => Ok(*f),
            _ => Err(unexpected!("Number", format!("{:?} to as_float", self))),
        }
    }
    fn as_atom(&self) -> Result<Form> {
        match self {
            Self::Symbol(..) => Ok(Form::Atom(Atomic::Symbol(self.as_boxed()))),
            Self::Number(..) => Ok(Form::Atom(Atomic::Number(self.as_boxed()))),
            Self::Exp(Expressable::Atom(a)) => Ok(*a.clone()),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "become atomic".to_string(),
            )),
        }
    }
    fn as_exp(&self) -> Result<Form> {
        match self {
            Self::Atom(..) => Ok(Self::Exp(Expressable::Atom(self.as_boxed()))),
            Self::List(..) => Ok(Self::Exp(Expressable::List(self.as_boxed()))),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "become an Expression".to_string(),
            )),
        }
    }
    fn as_list(&self) -> Result<Form> {
        match self {
            Self::List(..) => Ok(self.clone()),
            Self::Exp(Expressable::List(l)) => Ok(*l.clone()),
            _ => Ok(list!(self.clone())),
        }
    }
    fn as_vec(&self) -> Result<Vec<Form>> {
        match self.as_list()? {
            Form::List(l,..) => Ok(l),
            _ => Err(unexpected!("List", format!("{:?}", self))),
        }
    }
    fn as_boxed(&self) -> Box<Form> {
        Box::new(self.clone())
    }
    fn head(&self) -> Result<Form> {
        if !self.is_list() {
            Err(internal!("cannot call head on a non list"))
        } else {
            self.clone().nth_or_err(0)
        }
    }
    fn tail(&self) -> Result<Form> {
        if !self.is_list() {
            Err(internal!(&format!("{} {}", "head", NON_LIST_FN)))
        } else {
            match self {
                Form::List(lst,ti) => Ok(Form::List(
                    lst[1..].to_vec(),
                    ti.clone()
                )),
                _ => Err(internal!("Unreachable")),
            }
        }
    }
    fn nth(&self, i: usize) -> Result<Option<Form>> {
        if !self.is_list() {
            return Ok(None);
        }
        match self {
            Self::List(elements,..) => {
                if i > elements.len() {
                    return Ok(None);
                }
                Ok(Some(elements[i].clone()))
            }
            _ => Err(unexpected!("List", format!("{:?}", self))),
        }
    }
    fn nth_or_err(&self, i: usize) -> Result<Form> {
        match self.nth(i)? {
            Some(t) => Ok(t),
            _ => Err(internal!("nth_or")),
        }
    }
}
trait Peloteur: DynClone {
    fn get_param_types(&self,root: Form, manager: &TypeDeclManager) -> Result<TypeDecl>;
    fn can(&self, root: Form) -> Result<bool>;
    fn is_valid(&self, root: Form, manager: &TypeDeclManager) -> Result<bool>;
    fn eval(&self, root: Form, env: &mut Environment) -> Result<Form>;
    //fn get_param_types(&self, root: Form, env: &mut Environment) -> Result<bool>;
    //fn get_result_types(&self, root: Form, env: &mut Environment) -> Result<bool>;
}
dyn_clone::clone_trait_object!(Peloteur);
#[derive(Clone)]
struct Environment {
    type_manager: TypeDeclManager,
    vars: HashMap<String, Form>,
    peloteurs: Vec<Box<dyn Peloteur>>,
}
impl Environment {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            peloteurs: vec![],
            type_manager: TypeDeclManager::new(),
        }
    }
    fn register(&mut self, peloteur: Box<dyn Peloteur>) {
        // turns out putting them in a hash table is not trivial
        self.peloteurs.push(peloteur);
    }
    fn lookup(&self, key: &str) -> Result<Option<Form>> {
        match self.vars.get(key) {
            Some(t) => Ok(Some(t.clone())),
            _ => Ok(None),
        }
    }
    fn add(mut self, key: &str, value: Form) -> Self {
        self.vars.insert(key.to_string(), value);
        self
    }
}
fn tokenize(text: &str) -> Vec<String> {
    text.replace('(', " ( ")
        .replace(')', " ) ")
        .split(' ')
        .into_iter()
        .filter(|s| !s.trim().is_empty())
        .into_iter()
        .map(|s| s.to_string())
        .collect()
}
fn atomize(token: String) -> Result<Form> {
    match &token.parse::<i64>() {
        Ok(n) => Ok(int!(*n).as_atom()?),
        Err(..) => match &token.parse::<f64>() {
            Ok(f) => Ok(float!(*f).as_atom()?),
            Err(..) => {
                let sym = sym!(token.clone());
                if sym.is_true()? {
                    Ok(bool_t!())
                } else if sym.is_false()? {
                    Ok(bool_f!())
                } else {
                    Ok(sym!(token.clone()).as_atom()?)
                }
            }
        },
    }
}
fn is_lparen(s: &str) -> bool {
    let lparens = vec!["(","{","[","<"];
    if lparens.contains(&s) {
        return true;
    }
    false
}
fn is_rparen(s: &str) -> bool {
    let rparens = vec![")","}","]",">"];
    if rparens.contains(&s) {
        return true;
    }
    false
}
fn read_from_tokens(tokens: &[String], start: usize) -> Result<(Form, usize)> {
    if tokens.is_empty() {
        return Err(LustError::Syntax(
            "expected more than 0 tokens!".to_string(),
        ));
    }
    let mut i = start;
    if i > tokens.len() {
        return Err(internal!(format!(
            "{} is out of range for token count {}",
            i,
            tokens.len()
        )));
    }
    if is_lparen(&*tokens[i]) {
        let mut list = vec![];
        i += 1;
        while !is_rparen(&*tokens[i]) {
            let (token, i2) = read_from_tokens(tokens, i)?;
            list.push(token);
            i = i2;
        }
        i += 1;
        Ok((Form::List(list,TypeInfo::default()).as_exp()?, i))
    } else if is_rparen(&*tokens[i]) {
        Err(unexpected!("anything", ")"))
    } else {
        Ok((atomize(tokens[i].clone())?.as_exp()?, i + 1))
    }
}
#[derive(Clone)]
struct DoPeloteur {}
fn eval(exp: Form, env: &mut Environment) -> Result<Form> {
    match exp {
        Form::Symbol(ref s, ..) => {
            if exp.is_true()? || exp.is_false()? {
                Ok(exp)
            } else {
                match env.lookup(s)? {
                    Some(sym) => Ok(sym),
                    _ => Ok(exp),
                }
            }
        }
        Form::Number(..) => Ok(exp),
        Form::Atom(Atomic::Symbol(..)) => eval(exp.as_symbol()?, env),
        Form::Atom(Atomic::Number(..)) => eval(exp.as_number()?, env),
        Form::Atom(Atomic::Boolean(..)) => Ok(exp),
        Form::Exp(Expressable::Atom(..)) => eval(exp.as_atom()?, env),
        Form::Exp(Expressable::List(..)) => eval(exp.as_list()?, env),
        Form::List(..) => {
            for peloteur in &env.clone().peloteurs {
                if peloteur.can(exp.clone())? && peloteur.is_valid(exp.clone(), &env.type_manager)? {
                    return peloteur.eval(exp, env);
                }
            }
            Err(internal!(format!("don't know how to eval {:?}", exp)))
        }
        _ => Err(internal!(format!("don't know how to eval {:?}", exp))),
    }
}
fn register_default_peloteurs(env: &mut Environment) {
    env.register(Box::new(peloteurs::conditionals::If {}));
    env.register(Box::new(peloteurs::math::Math {}));
    env.register(Box::new(peloteurs::logic::Logic {}));
    env.register(Box::new(peloteurs::lambda::Lambda {}));
}
#[cfg(test)]
mod tests {
// TODO:
// * Implement let for variables
// * Implement Map
// * Implement Filter
// * Implement Fold
// * Implement Zip
// * Peloteurs need a method to indicate their return types based on the type passed in
// * Implement type system checking
// * Implement abs, min, and max
// * Implement file handling
    use super::*;
    #[test]
    fn test_typedecl_comparison_lambda() -> Result<()> {
        let t1 = TypeDecl::TUnion(vec![TypeDecl::TInt64, TypeDecl::TUser(33)]);
        let t2 = TypeDecl::TUnion(vec![TypeDecl::TUser(33),TypeDecl::TInt64]);
        let t3 = TypeDecl::TListOf(vec![TypeDecl::TSymbol,TypeDecl::TString]);
        let l1 = TypeDecl::TLambdaOf(t1.as_boxed(),t2.as_boxed());
        let l2 = TypeDecl::TLambdaOf(t1.as_boxed(),t3.as_boxed());
        let l3 = TypeDecl::TLambdaOf(t1.as_boxed(),t2.as_boxed());
        assert!(!l1.same_as(&l2));
        assert!(l3.same_as(&l1));
        Ok(())
    }
    #[test]
    fn test_typedecl_comparison() -> Result<()> {
        let t1 = TypeDecl::TUnion(vec![TypeDecl::TInt64, TypeDecl::TUser(33)]);
        let t2 = TypeDecl::TUnion(vec![TypeDecl::TUser(33),TypeDecl::TInt64]);
        let t3 = TypeDecl::TListOf(vec![TypeDecl::TSymbol,TypeDecl::TString]);
        let t4 = TypeDecl::TListOf(vec![TypeDecl::TString, TypeDecl::TSymbol]);
        assert!(t1.same_as(&t2));
        assert!(!t3.same_as(&t4), "same_as {:?} == {:?}", t3.to_ord(),t4.to_ord());
        assert!(t3.similar_to(&t4));
        Ok(())
    }
    #[test]
    fn test_form_get_param_types() -> Result<()> {
        let mut manager = TypeDeclManager::new();
        let i = manager.add_str("CustomType");
        let lambda = list!( // (lambda (a i_t) (i_t) (+ a 1))
            sym!("fn"),
            list!(
                sym!("a"),
                sym!("i_t"),
                sym!("b"),
                sym!("CustomType"),
            ),
            list!(
                sym!("i_t"),
            ),
            list!(
                sym!("+"),
                sym!("a"),
                int!(1),
            )
        );
        let typ = manager.parse_form(lambda)?;
        match typ {
            TypeDecl::TLambdaOf(idef,odef) => {
               match *idef {
                    TypeDecl::TListOf(v) => assert_eq!(v, vec![TypeDecl::TInt64,TypeDecl::TUser(0)],"in def is wrong vec"),
                    _ => unreachable!("in def is wrong"),
               };
               match *odef {
                    TypeDecl::TListOf(v) => assert_eq!(v, vec![TypeDecl::TInt64],"out def is wrong vec"),
                    _ => unreachable!("out def is wrong"),
               }
            }
            _ => unreachable!(),
        }
        Ok(())
    }
    #[test]
    fn test_typedeclmanager_is_type_keyword() -> Result<()> {
        let mut manager = TypeDeclManager::new();
        let i = manager.add_str("CustomType");
        let types = vec!["any_t","i64_t","i_t","f64_t","f_t","str_t","s_t","bool_t","b_t","fn_t","list_t","l_t"];
        for v in &types {
            assert!(manager.is_type_keyword(sym!(v)),"v={}", v.clone());
        }
        assert!(!manager.is_type_keyword(sym!("any_tt")));
        assert!(manager.is_type_keyword(sym!("CustomType")));
        Ok(())
    }
    #[test]
    fn test_typedeclmanager() -> Result<()> {
        let mut manager = TypeDeclManager::new();
        manager.user_defined_types.push("CustomType".to_string());
        assert_eq!(manager.parse_str("CustomType").unwrap(),TypeDecl::TUser(0));
        let i = manager.add_str("CustomType");
        assert!(i.is_err());
        let i = manager.add_str("CustomType2");
        assert_eq!(i.unwrap(),1);
        Ok(())
    }
    #[test]
    fn test_type_comparison() -> Result<()> {
        let t1 = int!(1);
        let t2 = int!(2);
        assert!(t1.is_same_type_as(t2));
        let t3 = float!(1.0);
        assert!(!t1.is_same_type_as(t3));
        Ok(())
    }
    #[test]
    fn test_booleans() -> Result<()> {
        assert!(sym!("true").is_true()?);
        assert!(sym!("false").is_false()?);
        assert!(!sym!("false").is_true()?);
        Ok(())
    }
    #[test]
    fn test_eval() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        let sym = sym!("hello");
        env = env.add("hello", sym.clone());
        // It should look variables up
        assert_eq!(eval(sym, &mut env)?.as_string()?, "hello".to_string());
        let num = int!(1);
        // It should return atomic types as themselves
        assert_eq!(eval(num.clone(), &mut env)?.as_int()?, 1);
        let atom = num.as_atom()?;
        // Atoms should reduce to their atomic types
        assert_eq!(eval(atom.clone(), &mut env)?.as_int()?, 1);
        // exp's of atom should behave as an atomic type
        assert_eq!(eval(atom.clone().as_exp()?, &mut env)?.as_int()?, 1);
        Ok(())
    }

    #[test]
    fn test_read_list_from_tokens() -> Result<()> {
        let tokens = tokenize("(a b c)");
        let (exp, _) = read_from_tokens(&tokens, 0)?;
        assert_eq!(
            exp.as_list()?
                .nth(0)?
                .unwrap()
                .as_atom()?
                .as_symbol()?
                .as_string()?,
            "a".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_read_symbol_from_tokens() -> Result<()> {
        let tokens = vec!["abc".to_string()];
        let (exp, _) = read_from_tokens(&tokens, 0)?;
        assert_eq!(exp.as_atom()?.as_symbol()?.as_string()?, "abc".to_string());
        Ok(())
    }

    #[test]
    fn test_read_number_from_tokens() -> Result<()> {
        let tokens = vec!["1".to_string()];
        let (exp, _) = read_from_tokens(&tokens, 0)?;
        assert_eq!(exp.as_atom()?.as_number()?.as_int()?, 1);
        Ok(())
    }

    #[test]
    fn test_parse_float() -> Result<()> {
        let atom = atomize("3.4".to_string())?;
        assert_eq!(atom.as_number()?.as_float()?, 3.4);
        Ok(())
    }

    #[test]
    fn test_form_as_number() -> Result<()> {
        let atom = int!(1).as_atom()?;
        assert_eq!(atom.clone().as_number()?.as_int()?, 1);
        assert!(atom.as_number()?.is_type(TypeDecl::TInt64));
        let atom = float!(3.4).as_atom()?;
        assert_eq!(atom.clone().as_number()?.as_float()?, 3.4);
        assert!(atom.as_number()?.is_type(TypeDecl::TFloat64));
        Ok(())
    }

    #[test]
    fn test_form_as_symbol() -> Result<()> {
        let atom = sym!("x").as_atom()?;
        let sym = atom.as_symbol()?;
        match sym {
            Form::Symbol(s, _) => assert_eq!(&*s, "x"),
            _ => unreachable!(),
        }
        Ok(())
    }

    #[test]
    fn test_form_as_atom() -> Result<()> {
        let atom = sym!("x").as_atom()?;
        assert!(atom.is_atom());
        assert_eq!(atom.clone().as_symbol()?.as_string()?, "x".to_string());
        Ok(())
    }

    #[test]
    fn test_paren_comps() -> Result<()> {
        let tokens = tokenize("()");
        assert!(is_lparen(&*tokens[0]));
        assert!(is_rparen(&*tokens[1]));
        Ok(())
    }

    #[test]
    fn test_tokenize() -> Result<()> {
        let tokens = tokenize("(())");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], "(".to_string());
        let tokens = tokenize("( (   )      \n)  ");
        assert_eq!(tokens.len(), 4);
        Ok(())
    }

    #[test]
    fn test_parse() -> Result<()> {
        let tokens = tokenize("1");
        assert_eq!(tokens.len(), 1);
        let atom = atomize(tokens[0].clone())?;
        assert_eq!(atom.clone().as_number()?.as_int()?, 1);
        Ok(())
    }
}
