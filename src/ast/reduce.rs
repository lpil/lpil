use super::super::env::Env;
use super::super::error::Error;
use super::Node;


/// The result of attempting to reduce an AST Node
///
#[derive(Debug, PartialEq)]
pub enum Reduced {
    Some(Node),
    Err(Error),
    None,
}

/// When a symbol is reduced it is replaced with the value
/// of a variable set in the env with the same name.
///
/// If there is no variable with the same name it returns an error.
///
pub fn symbol(name: &String, env: &mut Env) -> Reduced {
    match env.get_var(name) {
        Some(value) => Reduced::Some(value.clone()),
        None => Reduced::Err(Error::UndefinedVariable(name.to_string())),
    }
}
