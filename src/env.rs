// TODO
#![allow(dead_code)]

use super::ast;
use std::collections::HashMap;
use std::cell::RefCell;


type Scope = HashMap<String, ast::Node>;
type Stack = Vec<Scope>;


/// Container for information on current environment of execution.
/// i.e. Defined functions, variables in scope, etc.
///
#[derive(Debug)]
pub struct Env {
    // The current scope
    scope: RefCell<Scope>,

    // The stack of parent scopes
    stack: RefCell<Stack>,
}

impl Env {
    /// Construct a new Env with an empty scope and prelude functions.
    ///
    pub fn new() -> Env {
        Env {
            scope: RefCell::new(HashMap::new()),
            stack: RefCell::new(vec![]),
        }
    }

    /// Set a variable in the current scope.
    ///
    pub fn set_var(&mut self, k: String, v: ast::Node) {
        let mut scope = self.scope.borrow_mut();
        scope.insert(k, v);
    }

    /// Get a variable from the current scope.
    ///
    pub fn get_var(&mut self, k: &String) -> Option<ast::Node> {
        self.scope.borrow().get(k).map(|e| e.clone())
    }
}


#[cfg(test)]
mod tests {
    use super::super::ast;
    use super::*;

    #[test]
    fn set_and_get_var_in_env() {
        let mut env = Env::new();
        env.set_var("age".to_string(), ast::number(100.0));
        let res = env.get_var(&"age".to_string());
        assert_eq!(res, Some(ast::number(100.0)));
    }

    #[test]
    fn get_var_when_unset() {
        let mut env = Env::new();
        let res = env.get_var(&"santa".to_string());
        assert_eq!(res, None);
    }
}
