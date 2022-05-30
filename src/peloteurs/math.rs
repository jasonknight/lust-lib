use crate::*;
#[derive(Clone)]
pub struct Math {}
impl Peloteur for Math {
    fn get_param_types(&self, root: Form, manager: &TypeManager) -> Result<Type> {
        Ok(Type::TAny)
    }
    fn is_valid(&self, _root: Form, _manager: &TypeManager) -> Result<bool> {
        Ok(true)
    }
    fn can(&self, root: Form) -> Result<bool> {
        let h = root.as_list()?.head()?;
        let matches = h.matches_string("+")?
            || h.matches_string("-")?
            || h.matches_string("*")?
            || h.matches_string("/")?
            || h.matches_string("%")?;
        if !matches {
            Ok(false)
        } else {
            Ok(true)
        }
    }
    fn eval(&self, root: Form, env: &mut Environment) -> Result<Form> {
        let h = root.head()?;
        let list = root.as_list()?.as_vec()?;
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
        Ok(sum)
    }
}
mod tests {
    use super::*;
    // Float Math
    #[test]
    fn test_float_remainder() -> Result<()> {
        let mut env = Environment::new();
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
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
        register_default_peloteurs(&mut env);
        let list = list!(
            sym!("+"),
            int!(1),
            int!(1).as_atom()?,
        );

        assert_eq!(eval(list, &mut env)?.as_int()?, 2);
        Ok(())
    }
}
