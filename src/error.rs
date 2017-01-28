// TODO
#![allow(dead_code)]


use std::fmt::Write;


/// Possible runtime Errors
///
#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
}

impl Error {
    /// Prettily writes the error, suitable for display to the user.
    ///
    pub fn pretty_write(&self, b: &mut String) {
        match self {
            &Error::UndefinedVariable(ref name) => undef_msg(b, name),
        }
    }
}


fn undef_msg(b: &mut String, name: &String) {
    write!(b,
           r#"
Error: Undefined variable `{}`
"#,
           name)
        .unwrap()
}
