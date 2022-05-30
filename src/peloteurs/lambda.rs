use crate::*;
#[derive(Clone)]
pub struct Lambda {}
impl Peloteur for Lambda {
    fn get_param_types(&self, root: Form, manager: &TypeManager) -> Result<Type> {
        let list_root = root.as_list()?;
        let h = list_root.head()?;
        if sym_matches!(h, "fn") {
            Ok(Type::TListOf(vec![Type::TList,Type::TList,Type::TList]))
        } else if sym_matches!(h, "fn?") {
            Ok(Type::TUnion(vec![Type::TList,Type::TSymbol]))
        } else if sym_matches!(h, "applicable?") {
            Ok(Type::TList)
        } else if sym_matches!(h, "apply") {
            Ok(Type::TListOf(vec![Type::TUnion(vec![Type::TSymbol,Type::TLambda]),Type::TList]))
        } else {
            Err(internal!("cannot determine param types"))
        }
    }
    fn is_valid(&self, root: Form, manager: &TypeManager) -> Result<bool> {
        let list_root = root.as_list()?;
        let h = list_root.head()?;
        match &list_root {
            Form::List(lst,..) => match &lst[0] {
                Form::Symbol(..) => {
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
    fn can(&self, root: Form) -> Result<bool> {
        let h = root.as_list()?.head()?;
        Ok(sym_matches!(h, "fn?", "fn", "apply", "applicable?"))
    }
    fn eval(&self, root: Form, env: &mut Environment) -> Result<Form> {
        let list = root.as_list()?.as_vec()?;
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
                    } else { // must iter peloteurs
                        bool_result = false;
                        for hd in &env.peloteurs {
                            if hd.can(l.clone())? && hd.is_valid(l.clone(),&env.type_manager)? {
                                bool_result = true;
                            }
                        }
                    }
                } else if l.is_symbol() { // must iter peloteurs
                    bool_result = false;
                    for hd in &env.peloteurs {
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
                eval(Form::List(v,Info::default()), env)
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
                eval(body, &mut local_env)
            } else {
                Err(internal!("don't know what to do"))
            }
        } else {
            Err(internal!("something"))
        }
    }
}
mod tests {
    use super::*;
    #[test]
    fn test_lambda_peloteur_eval_apply_lambda() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        let peloteur = Lambda {};
        let lambda = list!(
            sym!("fn"),
            list!(sym!("x"), sym!("y"),),
            list!(sym!("+"), sym!("x"), sym!("y"), int!(1),)
        );
        let apply = list!(sym!("apply"), lambda.clone(), list!(int!(1), int!(2)),);
        assert_eq!(peloteur.eval(apply, &mut env)?.as_int()?, 4);
        Ok(())
    }

    #[test]
    fn test_lambda_peloteur_eval_apply_symbol() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        let peloteur = Lambda {};
        let apply = list!(sym!("apply"), sym!("+"), list!(int!(1), int!(2)),);
        assert_eq!(peloteur.eval(apply, &mut env)?.as_int()?, 3);
        Ok(())
    }

    #[test]
    fn test_lambda_peloteur_eval_lambda() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        let peloteur = Lambda {};
        let lambda = list!(sym!("fn"), list!(), list!(sym!("+"), int!(1), int!(2)));
        assert!(peloteur.eval(lambda.clone(), &mut env)?.is_lambda()?);

        Ok(())
    }
    #[test]
    fn test_lambda_peloteur_eval_lambdap() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        // A lambda just returns a lambda
        let peloteur = Lambda {};
        let lambda = list!(sym!("fn"), list!(), list!());
        let lambdap = list!(sym!("fn?"), lambda.clone(), lambda.clone(),);

        assert!(peloteur.eval(lambdap, &mut env)?.is_true()?);

        let lambdap_false = list!(sym!("fn?"), lambda.clone(), bool_t!(),);

        assert!(peloteur.eval(lambdap_false, &mut env)?.is_false()?);

        Ok(())
    }
    #[test]
    fn test_lambda_peloteur_eval_applicable() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        // A lambda just returns a lambda
        let peloteur = Lambda {};
        let lambda = list!(sym!("fn"), list!(), list!());
        let applyp = list!(
            sym!("applicable?"),
            lambda.clone(),
            list!(sym!("+"), int!(1), int!(2)),
            sym!("+"),
        );

        assert!(peloteur.eval(applyp, &mut env)?.is_true()?);

        let applyp_false = list!(
            sym!("applicable?"),
            list!(sym!("+"), int!(1), int!(2)),
            int!(1),
        );

        assert!(peloteur.eval(applyp_false, &mut env)?.is_false()?);

        Ok(())
    }
    #[test]
    fn test_lambda_peloteur_is_valid() -> Result<()> {
        let mut env = Environment::new();
        let peloteur = Lambda {};
        assert!(list!(sym!("fn")).is_lambda()?,"is_lambda");
        assert!(peloteur.is_valid(list!(sym!("fn")), &env.type_manager).is_err(),"is_err");
        assert!(
            peloteur.is_valid(
                list!(
                    sym!("fn"), 
                    list!(), // arguments 
                    list!(), // returns
                    list!(), // body
                ),
                &env.type_manager
            )?
        );
        assert!(
            peloteur.is_valid(
                list!(
                    sym!("fn"), 
                    list!(), // arguments 
                    //list!(), // returns 
                    list!(), // body
                ),
                &env.type_manager
            ).is_err()
        );
        assert!(peloteur.is_valid(list!(sym!("fn?"), list!(), list!()), &env.type_manager)?);
        assert!(peloteur
            .is_valid(list!(sym!("fn?"), list!(), list!(), sym!("lll")), &env.type_manager)
            .is_err());
        assert!(peloteur
            .is_valid(list!(sym!("apply"), list!(), int!(1)), &env.type_manager)
            .is_err());
        assert!(peloteur.is_valid(list!(sym!("apply"), list!(), list!()), &env.type_manager)?);
        Ok(())
    }

    #[test]
    fn test_lambda_peloteur() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
        let peloteur = Lambda {};
        assert!(peloteur.can(list!(sym!("fn")))?);
        assert!(peloteur.can(list!(sym!("fn?")))?);
        assert!(peloteur.can(list!(sym!("apply")))?);
        Ok(())
    }
}
