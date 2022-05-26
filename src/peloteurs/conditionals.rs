use crate::*;
#[derive(Clone)]
pub struct If {}
impl Peloteur for If {
    fn get_param_types(&self, root: Form, manager: &TypeDeclManager) -> Result<TypeDecl> {
        Ok(TypeDecl::TAny)
    }
    fn is_valid(&self, root: Form, _manager: &TypeDeclManager) -> Result<bool> {
        let list = root.as_list()?.as_vec()?;
        if list.len() != 4 {
            return Err(LustError::Semantic(
                "if".to_string(),
                "be called with > 3 arguments, predicate, consequent and alternative".to_string(),
            ));
        }
        Ok(true)
    }
    fn can(&self, root: Form) -> Result<bool> {
        let h = root.as_list()?.head()?;
        if sym_matches!(h, "if") {
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn eval(&self, root: Form, env: &mut Environment) -> Result<Form> {
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
mod tests {
    use super::*;
    #[test]
    fn test_eval_if_true() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);

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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
}
