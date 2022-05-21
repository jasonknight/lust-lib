use dyn_clone::DynClone;
use std::collections::HashMap;
use thiserror::Error;
use std::str::FromStr;

macro_rules! sym {
    ($s:expr) => {
        Type::Symbol($s.to_string(), TypeInfo::new(0,0,TypeDecl::TString))
    };
    ($s:expr,$l:expr,$c:expr) => {
        Type::Symbol(
            $s.to_string(),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TString,
            ),
        )
    };
}

macro_rules! int {
    ($i:expr) => {
        Type::Number(Numerical::Int64($i), TypeInfo::new(0, 0, TypeDecl::TInt64))
    };
    ($i:expr,$l:expr,$c:expr) => {
        Type::Number(
            Numerical::Int64($i),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TInt64,
            ),
        )
    };
}

macro_rules! float {
    ($i:expr) => {
        Type::Number(Numerical::Float64($i), TypeInfo::new(0,0,TypeDecl::TFloat64))
    };
    ($i:expr,$l:expr,$c:expr) => {
        Type::Number(
            Numerical::Float64($i),
            TypeInfo::new(
                $l,
                $c,
                TypeDecl::TFloat64,
            ),
        )
    };
}

macro_rules! bool_t {
    () => {
        Type::Atom(Atomic::Boolean(Logical::True, TypeInfo::new(0,0,TypeDecl::TBoolean)))
    };
}

macro_rules! bool_f {
    () => {
        Type::Atom(Atomic::Boolean(Logical::False, TypeInfo::new(0,0,TypeDecl::TBoolean)))
    };
}

macro_rules! list {
    () => (
        Type::List(Vec::new(),TypeInfo::default())
    );
    ($($x:expr),+ $(,)?) => (
        Type::List(vec![$($x),+],TypeInfo::default())
    );
}

macro_rules! sym_matches {
    ($h:expr, $($x:expr),+ $(,)?) => (
        vec![$($x),+].iter().fold(false, |r,c| r || $h.matches_string(c).unwrap_or(false) )
    );
}

/// Error Macros
macro_rules! internal {
    ($e:expr) => {
        LustError::Internal($e.to_string())
    };
}
macro_rules! syntax {
    ($e:expr) => {
        LustError::Syntax($e.to_string())
    };
}
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
    TAny,
    TInt64,
    TFloat64,
    TString,
    TBoolean,
    TLambda,
    TList,
    TListOf(Vec<TypeDecl>),
    TUnion(Vec<TypeDecl>),
    TUser(usize), 
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
    fn from_str(&self,tstr: &str) -> Result<TypeDecl> {
        match tstr {
            "any_t" => Ok(TypeDecl::TAny),
            "i64_t" | "i_t" => Ok(TypeDecl::TInt64),
            "f64_t" | "f_t" => Ok(TypeDecl::TFloat64),
            "str_t" | "s_t" => Ok(TypeDecl::TString),
            "bool_t"| "b_t" => Ok(TypeDecl::TBoolean),
            "fn_t" => Ok(TypeDecl::TLambda),
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
    fn from_type(&self,typ: Type) -> Result<TypeDecl> {
        match typ {
            Type::List(lst,..) => {
                let mut types = vec![];
                for t in &lst {
                    types.push(self.from_type(t.clone())?);
                }
                Ok(TypeDecl::TListOf(types))
            },
            Type::Symbol(s,..) => Ok(self.from_str(&s)?),
            _ => Err(internal!(format!("cannot convert {:?} to a TypeDecl",typ))),
        }
    }
    fn is_type_keyword(&self, typ: Type) -> bool {
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
    Symbol(Box<Type>),
    Number(Box<Type>),
    Boolean(Logical, TypeInfo),
}
#[derive(Debug, Clone)]
enum Expressable {
    Atom(Box<Type>),
    List(Box<Type>),
}
#[derive(Debug, Clone)]
enum Type {
    Symbol(String, TypeInfo),
    Number(Numerical, TypeInfo),
    /// Symbol, Number
    Atom(Atomic),
    List(Vec<Type>,TypeInfo),
    /// Atom, List
    Exp(Expressable),
}
impl Type {
    fn get_decl_type(&self) -> TypeDecl {
        match self {
            Self::Atom(Atomic::Number(ref n)) => {
               if let Self::Number(_,ref info) = **n {
                   info.decl.clone()
               } else {
                    panic!("wtf");
               }
            }, 
            Self::Number(_,info) => info.decl.clone(),
            Self::List(..) => TypeDecl::TList,
            Self::Symbol(_,info) => info.decl.clone(),
            Self::Exp(Expressable::Atom(a)) => a.get_decl_type(),
            Self::Exp(Expressable::List(_)) => TypeDecl::TList,
            _ => panic!("wtf"),
       }
    }
    fn is_same_type_as(&self,other:Type) -> bool {
       let self_type = self.get_decl_type();
       let other_type = other.get_decl_type();
       self_type == other_type
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
        if self.clone().as_string()? == s.to_string() {
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
    fn as_symbol(self) -> Result<Type> {
        match self.clone() {
            Self::Symbol(..) => Ok(self),
            Self::Atom(Atomic::Symbol(s)) => Ok(*s),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "be reduced to a Symbol".to_string(),
            )),
        }
    }
    fn as_number(self) -> Result<Type> {
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
    fn as_atom(&self) -> Result<Type> {
        match self {
            Self::Symbol(..) => Ok(Type::Atom(Atomic::Symbol(self.as_boxed()))),
            Self::Number(..) => Ok(Type::Atom(Atomic::Number(self.as_boxed()))),
            Self::Exp(Expressable::Atom(a)) => Ok(*a.clone()),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "become atomic".to_string(),
            )),
        }
    }
    fn as_exp(&self) -> Result<Type> {
        match self {
            Self::Atom(..) => Ok(Self::Exp(Expressable::Atom(self.as_boxed()))),
            Self::List(_,..) => Ok(Self::Exp(Expressable::List(self.as_boxed()))),
            _ => Err(LustError::Semantic(
                format!("{:?}", self),
                "become an Expression".to_string(),
            )),
        }
    }
    fn as_list(&self) -> Result<Type> {
        match self {
            Self::List(_,..) => Ok(self.clone()),
            Self::Exp(Expressable::List(l)) => Ok(*l.clone()),
            _ => Ok(list!(self.clone())),
        }
    }
    fn as_vec(&self) -> Result<Vec<Type>> {
        match self.as_list()? {
            Type::List(l,..) => Ok(l),
            _ => Err(unexpected!("List", format!("{:?}", self))),
        }
    }
    fn as_boxed(&self) -> Box<Type> {
        Box::new(self.clone())
    }
    fn head(&self) -> Result<Type> {
        if !self.is_list() {
            Err(internal!("cannot call head on a non list"))
        } else {
            self.clone().nth_or_err(0)
        }
    }
    fn tail(&self) -> Result<Type> {
        if !self.is_list() {
            Err(internal!(&format!("{} {}", "head", NON_LIST_FN)))
        } else {
            match self {
                Type::List(lst,ti) => Ok(Type::List(
                    lst[1..].iter().map(|t| t.clone()).collect::<Vec<Type>>(),
                    ti.clone()
                )),
                _ => Err(internal!("Unreachable")),
            }
        }
    }
    fn nth(&self, i: usize) -> Result<Option<Type>> {
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
    fn nth_or_err(&self, i: usize) -> Result<Type> {
        match self.nth(i)? {
            Some(t) => Ok(t),
            _ => Err(internal!("nth_or")),
        }
    }
}
trait Handler: DynClone {
    fn can(&self, root: Type) -> Result<bool>;
    fn is_valid(&self, root: Type) -> Result<bool>;
    fn eval(&self, root: Type, env: &mut Environment) -> Result<Type>;
}
dyn_clone::clone_trait_object!(Handler);
#[derive(Clone)]
struct Environment {
    type_manager: TypeDeclManager,
    vars: HashMap<String, Type>,
    handlers: Vec<Box<dyn Handler>>,
}
impl Environment {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            handlers: vec![],
            type_manager: TypeDeclManager::new(),
        }
    }
    fn register(&mut self, handler: Box<dyn Handler>) {
        // turns out putting them in a hash table is not trivial
        self.handlers.push(handler);
    }
    fn lookup(&self, key: &str) -> Result<Option<Type>> {
        match self.vars.get(key) {
            Some(t) => Ok(Some(t.clone())),
            _ => Ok(None),
        }
    }
    fn add(mut self, key: &str, value: Type) -> Self {
        self.vars.insert(key.to_string(), value);
        self
    }
}
fn tokenize(text: &str) -> Vec<String> {
    text.replace("(", " ( ")
        .replace(")", " ) ")
        .split(" ")
        .into_iter()
        .filter(|s| !s.trim().is_empty())
        .into_iter()
        .map(|s| s.to_string())
        .collect()
}
fn atomize(token: String) -> Result<Type> {
    match &token.parse::<i64>() {
        Ok(n) => Ok(int!(*n).as_atom()?),
        Err(_) => match &token.parse::<f64>() {
            Ok(f) => Ok(float!(*f).as_atom()?),
            Err(_) => {
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
fn read_from_tokens(tokens: &[String], start: usize) -> Result<(Type, usize)> {
    if tokens.len() == 0 {
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
        return Ok((Type::List(list,TypeInfo::default()).as_exp()?, i));
    } else if is_rparen(&*tokens[i]) {
        return Err(unexpected!("anything", ")"));
    } else {
        return Ok((atomize(tokens[i].clone())?.as_exp()?, i + 1));
    }
}
#[derive(Clone)]
struct DoHandler {}
#[derive(Clone)]
struct LambdaHandler {}
impl Handler for LambdaHandler {
    fn is_valid(&self, root: Type) -> Result<bool> {
        let list_root = root.as_list()?;
        let h = list_root.clone().head()?;
        match &list_root {
            Type::List(lst,..) => match &lst[0] {
                Type::Symbol(_, _) => {
                    if sym_matches!(h, "fn") {
                        if lst.len() != 4 { // (fn (a t b t c t) (t) (+ a b c)
                            Err(
                                internal!(
                                    "fn expects 3 arguments, a list of arguments, a list of return types, and an executable list"
                                )
                            )
                        } else if !lst[1].clone().is_list() && lst[2].clone().is_list() && lst[3].clone().is_list() {
                            // We check the argument base type
                            Err(internal!(format!(
                                "the 3 arguments to fn must be lists got {:?}, {:?} and {:?}",
                                lst[1], lst[2], lst[3]
                            )))
                        } else {
                            Ok(true)
                        }
                    } else if sym_matches!(h, "fn?") {
                        for l in &lst[1..] {
                            if !l.is_list() {
                                return Err(internal!(format!(
                                    "arguments to fn? must be lists, got {:?}",
                                    l
                                )));
                            }
                        }
                        Ok(true)
                    } else if sym_matches!(h, "apply", "applicable?") {
                        for l in &lst[1..] {
                            if !l.is_list() && !l.is_symbol() {
                                return Err(internal!(format!(
                                    "arguments to apply must be lists or symbols, got {:?}",
                                    l
                                )));
                            }
                        }
                        Ok(true)
                    } else {
                        Ok(true)
                    }
                }
                _ => Ok(false),
            },
            _ => Ok(false),
        }
    }
    fn can(&self, root: Type) -> Result<bool> {
        let h = root.as_list()?.head()?;
        Ok(sym_matches!(h, "fn?", "fn", "apply", "applicable?"))
    }
    fn eval(&self, root: Type, env: &mut Environment) -> Result<Type> {
        let list = root.clone().as_list()?.as_vec()?;
        let h = root.head()?;
        if sym_matches!(h, "fn") {
            Ok(root.as_list()?)
        } else if sym_matches!(h, "fn?") {
            let mut bool_result = true;
            for l in &list[1..] {
                if !l.is_lambda()? {
                    bool_result = false;
                    break;
                }
            }
            if bool_result {
                Ok(bool_t!())
            } else {
                Ok(bool_f!())
            }
        } else if sym_matches!(h, "applicable?") {
            let mut bool_result = true;
            for l in &list[1..] {
                bool_result = false;
                if l.is_list() {
                    if l.is_lambda()? {
                        bool_result = true;
                    } else { // must iter handlers
                        bool_result = false;
                        for hd in &env.handlers {
                            if hd.can(l.clone())? && hd.is_valid(l.clone())? {
                                bool_result = true;
                            }
                        }
                    }
                } else if l.is_symbol() { // must iter handlers
                    bool_result = false;
                    for hd in &env.handlers {
                        if hd.can(l.clone())? {
                            bool_result = true;
                        }
                    }
                }
            }
            if bool_result {
                Ok(bool_t!())
            } else {
                Ok(bool_f!())
            }
        } else if sym_matches!(h, "apply") {
            if list[1].is_symbol() { // (apply + (1 2)
                let mut v = vec![list[1].clone()];
                for el in &list[2].clone().as_vec()? {
                    v.push(el.clone());
                }
                return eval(Type::List(v,TypeInfo::default()), env);
            } else if list[1].is_lambda()? { // (apply (fn (a b) (+ a b)) (1 2))
                let lambda = list[1].clone();
                let arg_values = list[2].clone();
                let arg_list = lambda.tail()?.head()?;
                let body = lambda.tail()?.tail()?.head()?;
                let mut local_env = env.clone();
                let mut i = 0;
                for sym in &arg_list.as_vec()? {
                    let name = sym.clone().as_string()?;
                    let value = eval(arg_values.nth_or_err(i)?, env)?;
                    local_env = local_env.add(&name, value);
                    i += 1;
                }
                return eval(body, &mut local_env);
            } else {
                Err(internal!("don't know what to do"))
            }
        } else {
            Err(internal!("something"))
        }
    }
}
#[derive(Clone)]
struct OpHandler {}
impl Handler for OpHandler {
    fn is_valid(&self, _root: Type) -> Result<bool> {
        Ok(true)
    }
    fn can(&self, root: Type) -> Result<bool> {
        let h = root.as_list()?.head()?;
        Ok(sym_matches!(h, "lt?", "gt?", "eq?", "and?", "or?", "xor?"))
    }
    fn eval(&self, root: Type, env: &mut Environment) -> Result<Type> {
        let list = root.clone().as_list()?.as_vec()?;
        if list.len() < 3 {
            return Err(internal!("ops require minimum 2 arguments".to_string()));
        }
        let h = root.head()?;
        let mut i = 2;
        let mut bool_result = true;
        // This could be made faster with in comparison
        while i < list.len() {
            let left = eval(list[i - 1].clone(), env)?;
            //let cleft = left.clone();
            let right = eval(list[i].clone(), env)?;
            //let cright = right.clone();
            if h.matches_string("lt?")? {
                if left.is_int() {
                    bool_result = bool_result && (left.as_int()? < right.as_int()?);
                } else if left.is_float() {
                    bool_result = bool_result && (left.as_float()? < right.as_float()?);
                }
            } else if h.matches_string("gt?")? {
                if left.is_int() {
                    bool_result = bool_result && (left.as_int()? > right.as_int()?);
                } else if left.is_float() {
                    bool_result = bool_result && (left.as_float()? > right.as_float()?);
                }
            } else if h.matches_string("eq?")? {
                if left.is_int() {
                    bool_result = bool_result && (left.as_int()? == right.as_int()?);
                } else if left.is_float() {
                    bool_result = bool_result && (left.as_float()? == right.as_float()?);
                } else if left.is_symbol() {
                    bool_result = bool_result && (left.as_string()? == right.as_string()?);
                }
            } else if h.matches_string("and?")? {
                bool_result = true;
                if left.is_bool()? && right.is_bool()? {
                    if !(left.as_bool()? && right.as_bool()?) {
                        bool_result = false; // i.e. short-circuit
                        break;
                    }
                } else {
                    return Err(internal!(format!("and? can only be used with boolean, got {:?} and {:?}",left,right)));
                }
            } else if h.matches_string("or?")? {
                if left.is_bool()? && right.is_bool()? {
                    bool_result = bool_result && (left.as_bool()? || right.as_bool()?);
                }
            } else if h.matches_string("xor?")? && left.is_bool()? && right.is_bool()? {
                if (left.is_true()? && right.is_true()?)
                    || (left.is_false()? && right.is_false()?)
                {
                    bool_result = false;
                } else if left.as_bool()? || right.as_bool()? {
                    bool_result = true;
                }
            }
            i += 1;
        }
        if bool_result {
            Ok(bool_t!())
        } else {
            Ok(bool_f!())
        }
    }
}
#[derive(Clone)]
struct ArithHandler {}
impl Handler for ArithHandler {
    fn is_valid(&self, _root: Type) -> Result<bool> {
        Ok(true)
    }
    fn can(&self, root: Type) -> Result<bool> {
        let h = root.as_list()?.head()?;
        let matches = h.matches_string("+")?
            || h.matches_string("-")?
            || h.matches_string("*")?
            || h.matches_string("/")?
            || h.matches_string("%")?;
        if matches == false {
            Ok(false)
        } else {
            Ok(true)
        }
    }
    fn eval(&self, root: Type, env: &mut Environment) -> Result<Type> {
        let h = root.head()?;
        let list = root.clone().as_list()?.as_vec()?;
        if list.len() < 3 {
            return Err(unexpected!(
                "at least 2 arguments",
                format!("{}", list.len() - 1)
            ));
        }
        let nth = root.nth_or_err(1)?;
        let mut sum = eval(nth, env)?.as_number()?;
        let mut i = 2;
        while i < list.len() {
            let cur = eval(root.nth_or_err(i)?, env)?;

            if sum.is_int() {
                if h.matches_string("+")? {
                    sum = int!(sum.as_int()? + cur.as_int()?);
                } else if h.matches_string("-")? {
                    sum = int!(sum.as_int()? - cur.as_int()?);
                } else if h.matches_string("*")? {
                    sum = int!(sum.as_int()? * cur.as_int()?);
                } else if h.matches_string("/")? {
                    let tmp = cur.as_int()?;
                    if tmp == 0 {
                        return Err(internal!("division by 0"));
                    }
                    sum = int!(sum.as_int()? / tmp);
                } else if h.matches_string("%")? {
                    sum = int!(sum.as_int()? % cur.as_int()?);
                }
            } else if sum.is_float() {
                if h.matches_string("+")? {
                    sum = float!(sum.as_float()? + cur.as_float()?);
                } else if h.matches_string("-")? {
                    sum = float!(sum.as_float()? - cur.as_float()?);
                } else if h.matches_string("*")? {
                    sum = float!(sum.as_float()? * cur.as_float()?);
                } else if h.matches_string("/")? {
                    let tmp = cur.as_float()?;
                    if tmp == 0.0 {
                        return Err(internal!("division by 0"));
                    }
                    sum = float!(sum.as_float()? / tmp);
                } else if h.matches_string("%")? {
                    sum = float!(sum.as_float()? % cur.as_float()?);
                }
            }

            i += 1;
        }
        return Ok(sum);
    }
}

#[derive(Clone)]
struct IfHandler {}
impl Handler for IfHandler {
    fn is_valid(&self, root: Type) -> Result<bool> {
        let list = root.as_list()?.as_vec()?;
        if list.len() != 4 {
            return Err(LustError::Semantic(
                "if".to_string(),
                "be called with > 3 arguments, predicate, consequent and alternative".to_string(),
            ));
        }
        Ok(true)
    }
    fn can(&self, root: Type) -> Result<bool> {
        let h = root.as_list()?.head()?;
        if sym_matches!(h, "if") {
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn eval(&self, root: Type, env: &mut Environment) -> Result<Type> {
        let list = root.as_list()?.as_vec()?;
        let pred = eval(list[1].clone(), env)?;
        if pred.is_true()? {
            eval(list[2].clone(), env)
        } else if pred.is_false()? {
            eval(list[3].clone(), env)
        } else {
            Err(LustError::Syntax(
                "a predicate can only reduce to true or false".to_string(),
            ))
        }
    }
}
fn eval(exp: Type, env: &mut Environment) -> Result<Type> {
    match exp {
        Type::Symbol(ref s, ..) => {
            if exp.is_true()? || exp.is_false()? {
                Ok(exp)
            } else {
                match env.lookup(s)? {
                    Some(sym) => Ok(sym),
                    _ => Ok(exp),
                }
            }
        }
        Type::Number(..) => Ok(exp),
        Type::Atom(Atomic::Symbol(..)) => eval(exp.as_symbol()?, env),
        Type::Atom(Atomic::Number(..)) => eval(exp.as_number()?, env),
        Type::Atom(Atomic::Boolean(..)) => Ok(exp),
        Type::Exp(Expressable::Atom(_)) => eval(exp.as_atom()?, env),
        Type::Exp(Expressable::List(_)) => eval(exp.as_list()?, env),
        Type::List(_,..) => {
            for handler in &env.clone().handlers {
                if handler.can(exp.clone())? && handler.is_valid(exp.clone())? {
                    return handler.eval(exp, env);
                }
            }
            Err(internal!(format!("don't know how to eval {:?}", exp)))
        }
        _ => Err(internal!(format!("don't know how to eval {:?}", exp))),
    }
}
fn register_default_handlers(env: &mut Environment) {
    env.register(Box::new(IfHandler {}));
    env.register(Box::new(ArithHandler {}));
    env.register(Box::new(OpHandler {}));
    env.register(Box::new(LambdaHandler {}));
}
#[cfg(test)]
mod tests {
    // (let my-fn 
    //   (fn (a i_t b i_t) (i_t) 
    //     (+ a b)))
    use super::*;
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
        assert_eq!(manager.from_str("CustomType").unwrap(),TypeDecl::TUser(0));
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
    fn test_lambda_handler_eval_apply_lambda() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let handler = LambdaHandler {};
        let lambda = list!(
            sym!("fn"),
            list!(sym!("x"), sym!("y"),),
            list!(sym!("+"), sym!("x"), sym!("y"), int!(1),)
        );
        let apply = list!(sym!("apply"), lambda.clone(), list!(int!(1), int!(2)),);
        assert_eq!(handler.eval(apply, &mut env)?.as_int()?, 4);
        Ok(())
    }

    #[test]
    fn test_lambda_handler_eval_apply_symbol() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let handler = LambdaHandler {};
        let apply = list!(sym!("apply"), sym!("+"), list!(int!(1), int!(2)),);
        assert_eq!(handler.eval(apply, &mut env)?.as_int()?, 3);
        Ok(())
    }

    #[test]
    fn test_lambda_handler_eval_lambda() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let handler = LambdaHandler {};
        let lambda = list!(sym!("fn"), list!(), list!(sym!("+"), int!(1), int!(2)));
        assert!(handler.eval(lambda.clone(), &mut env)?.is_lambda()?);

        Ok(())
    }
    #[test]
    fn test_lambda_handler_eval_lambdap() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        // A lambda just returns a lambda
        let handler = LambdaHandler {};
        let lambda = list!(sym!("fn"), list!(), list!());
        let lambdap = list!(sym!("fn?"), lambda.clone(), lambda.clone(),);

        assert!(handler.eval(lambdap, &mut env)?.is_true()?);

        let lambdap_false = list!(sym!("fn?"), lambda.clone(), bool_t!(),);

        assert!(handler.eval(lambdap_false, &mut env)?.is_false()?);

        Ok(())
    }
    #[test]
    fn test_lambda_handler_eval_applicable() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        // A lambda just returns a lambda
        let handler = LambdaHandler {};
        let lambda = list!(sym!("fn"), list!(), list!());
        let applyp = list!(
            sym!("applicable?"),
            lambda.clone(),
            list!(sym!("+"), int!(1), int!(2)),
            sym!("+"),
        );

        assert!(handler.eval(applyp, &mut env)?.is_true()?);

        let applyp_false = list!(
            sym!("applicable?"),
            list!(sym!("+"), int!(1), int!(2)),
            int!(1),
        );

        assert!(handler.eval(applyp_false, &mut env)?.is_false()?);

        Ok(())
    }
    #[test]
    fn test_lambda_handler_is_valid() -> Result<()> {
        let handler = LambdaHandler {};
        assert!(list!(sym!("fn")).is_lambda()?,"is_lambda");
        assert!(handler.is_valid(list!(sym!("fn"))).is_err(),"is_err");
        assert!(
            handler.is_valid(
                list!(
                    sym!("fn"), 
                    list!(), // arguments 
                    list!(), // returns
                    list!(), // body
                )
            )?
        );
        assert!(
            handler.is_valid(
                list!(
                    sym!("fn"), 
                    list!(), // arguments 
                    //list!(), // returns 
                    list!(), // body
                )
            ).is_err()
        );
        assert!(handler.is_valid(list!(sym!("fn?"), list!(), list!()))?);
        assert!(handler
            .is_valid(list!(sym!("fn?"), list!(), list!(), sym!("lll")))
            .is_err());
        assert!(handler
            .is_valid(list!(sym!("apply"), list!(), int!(1)))
            .is_err());
        assert!(handler.is_valid(list!(sym!("apply"), list!(), list!()))?);
        Ok(())
    }

    #[test]
    fn test_lambda_handler() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let handler = LambdaHandler {};
        assert!(handler.can(list!(sym!("fn")))?);
        assert!(handler.can(list!(sym!("fn?")))?);
        assert!(handler.can(list!(sym!("apply")))?);
        Ok(())
    }
    #[test]
    fn test_op_handler_xor() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);

        let list = list!(sym!("xor?"), bool_t!(), bool_t!(),);

        assert!(eval(list, &mut env)?.is_false()?);

        let list = list!(sym!("xor?"), bool_f!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);

        let list = list!(sym!("xor?"), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_true()?);
        Ok(())
    }

    #[test]
    fn test_op_handler_or() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);

        let list = list!(sym!("or?"), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("or?"), bool_f!(), bool_f!(), bool_f!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);
        Ok(())
    }

    #[test]
    fn test_op_handler_and() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);

        let list = list!(sym!("and?"), bool_t!(), bool_t!(),);

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("and?"), bool_t!(), bool_t!(), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);
        Ok(())
    }
    #[test]
    fn test_op_handler() -> Result<()> {
        let h = OpHandler {};
        let opts = vec!["lt?", "gt?", "eq?", "and?"];
        for o in opts.iter() {
            assert!(h.can(list!(sym!(o)))?);
        }

        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(sym!("lt?"), int!(5), int!(6),);

        assert!(eval(list, &mut env)?.is_true()?);
        let list = list!(sym!("lt?"), int!(6), int!(5),);

        assert!(eval(list, &mut env)?.is_false()?);

        let list = list!(sym!("gt?"), int!(6), int!(5),);

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("eq?"), int!(5), int!(5));

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("eq?"), int!(5), int!(3));

        assert!(eval(list, &mut env)?.is_false()?);

        // Equivalent to: (eq (+ 1 (/ 10 5)) 3)
        let list = list!(
            sym!("eq?"),
            list!(
                sym!("+"), 
                int!(1), 
                list!(
                    sym!("/"), 
                    int!(10), 
                    int!(5),
                ),
            ),
            int!(3),
        );

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("eq?"), sym!("hello"), sym!("hello"));

        assert!(eval(list, &mut env)?.is_true()?);
        env = env.add("$hello", sym!("world"));
        let list = list!(sym!("eq?"), sym!("$hello"), sym!("world"));

        assert!(eval(list, &mut env)?.is_true()?);

        Ok(())
    }
    // Float Math
    #[test]
    fn test_float_remainder() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("%"),
            float!(5.0),
            float!(3.0).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_float()?, 2.0);
        Ok(())
    }
    #[test]
    fn test_float_division() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("/"),
            float!(20.0),
            float!(5.0).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_float()?, 4.0);

        let list = list!(
            sym!("/"),
            float!(20.0),
            float!(0.0).as_atom()?,
        );

        assert!(eval(list, &mut env).is_err());
        Ok(())
    }
    #[test]
    fn test_float_multiplication() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("*"),
            float!(1.0),
            float!(5.0).as_atom()?,
            float!(5.0).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_float()?, 25.0);
        Ok(())
    }
    #[test]
    fn test_float_subtraction() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("-"),
            float!(3.0),
            float!(2.0).as_atom()?,
        );
        assert_eq!(eval(list, &mut env)?.as_float()?, 1.0);
        Ok(())
    }
    #[test]
    fn test_float_addition() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("+"),
            float!(1.0),
            float!(1.0).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_float()?, 2.0);
        Ok(())
    }
    // Integer Math
    #[test]
    fn test_division() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("/"),
            int!(20),
            int!(5).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 4);

        let list = list!(
            sym!("/"),
            int!(20),
            int!(0).as_atom()?,
        );

        assert!(eval(list, &mut env).is_err());
        Ok(())
    }
    #[test]
    fn test_remainder() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("%"),
            int!(5),
            int!(3).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 2);
        Ok(())
    }
    #[test]
    fn test_multiplication() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("*"),
            int!(1),
            int!(5).as_atom()?,
            int!(5).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 25);
        Ok(())
    }

    #[test]
    fn test_subtraction() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("-"),
            int!(1),
            int!(1).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 0);
        Ok(())
    }

    #[test]
    fn test_addition() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("+"),
            int!(1),
            int!(1).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 2);
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
    fn test_eval_if_true() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);

        let list = list!(
            sym!("if"),
            bool_t!(),
            int!(1).as_atom()?,
            int!(2).as_atom()?
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 1);
        Ok(())
    }
    #[test]
    fn test_eval_if_false() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("if"),
            bool_f!(),
            int!(1).as_atom()?,
            int!(2).as_atom()?
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 2);
        Ok(())
    }
    #[test]
    fn test_eval_if_true_var() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
        let list = list!(
            sym!("if"),
            sym!("true_var"),
            int!(1).as_atom()?,
            int!(2).as_atom()?
        );
        env = env.add(
            "true_var",
            bool_t!(),
        );
        assert_eq!(eval(list.clone(), &mut env)?.as_int()?, 1);
        // Make sure an Exp => List
        assert_eq!(eval(list.as_exp()?, &mut env)?.as_int()?, 1);
        Ok(())
    }
    #[test]
    fn test_eval() -> Result<()> {
        let mut env = Environment::new();
        register_default_handlers(&mut env);
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
    fn test_type_as_number() -> Result<()> {
        let atom = int!(1).as_atom()?;
        assert_eq!(atom.as_number()?.as_int()?, 1);
        let atom = float!(3.4).as_atom()?;
        assert_eq!(atom.as_number()?.as_float()?, 3.4);
        Ok(())
    }

    #[test]
    fn test_type_as_symbol() -> Result<()> {
        let atom = sym!("x").as_atom()?;
        let sym = atom.as_symbol()?;
        match sym {
            Type::Symbol(s, _) => assert_eq!(&*s, "x"),
            _ => unreachable!(),
        }
        Ok(())
    }

    #[test]
    fn test_type_as_atom() -> Result<()> {
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
