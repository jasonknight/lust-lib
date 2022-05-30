use crate::*;
#[derive(Clone)]
pub struct Logic {}
impl Peloteur for Logic {
    fn get_param_types(&self, root: Form, manager: &TypeManager) -> Result<Type> {
        Ok(Type::TAny)
    }
    fn is_valid(&self, _root: Form, _manager: &TypeManager) -> Result<bool> {
        Ok(true)
    }
    fn can(&self, root: Form) -> Result<bool> {
        let h = root.as_list()?.head()?;
        Ok(sym_matches!(h, "lt?", "gt?", "eq?", "and?", "or?", "xor?"))
    }
    fn eval(&self, root: Form, env: &mut Environment) -> Result<Form> {
        let list = root.as_list()?.as_vec()?;
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
mod tests {
    use super::*;
    #[test]
    fn test_op_peloteur_xor() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);

        let list = list!(sym!("xor?"), bool_t!(), bool_t!(),);

        assert!(eval(list, &mut env)?.is_false()?);

        let list = list!(sym!("xor?"), bool_f!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);

        let list = list!(sym!("xor?"), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_true()?);
        Ok(())
    }

    #[test]
    fn test_op_peloteur_or() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);

        let list = list!(sym!("or?"), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("or?"), bool_f!(), bool_f!(), bool_f!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);
        Ok(())
    }

    #[test]
    fn test_op_peloteur_and() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);

        let list = list!(sym!("and?"), bool_t!(), bool_t!(),);

        assert!(eval(list, &mut env)?.is_true()?);

        let list = list!(sym!("and?"), bool_t!(), bool_t!(), bool_t!(), bool_f!(),);

        assert!(eval(list, &mut env)?.is_false()?);
        Ok(())
    }
    #[test]
    fn test_op_peloteur() -> Result<()> {
        let h = Logic {};
        let opts = vec!["lt?", "gt?", "eq?", "and?"];
        for o in opts.iter() {
            assert!(h.can(list!(sym!(o)))?);
        }

        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
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
}
