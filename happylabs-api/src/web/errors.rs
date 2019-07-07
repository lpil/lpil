use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
pub struct Errors {
    pub errors: HashMap<String, Vec<String>>,
}

impl Errors {
    pub fn single(key: String, error_message: String) -> Self {
        Self {
            errors: [(key, vec![error_message])].iter().cloned().collect(),
        }
    }
}
