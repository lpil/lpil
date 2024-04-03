use gotham::{
    handler::IntoResponse,
    state::{FromState, State},
};
use gotham_derive::{StateData, StaticResponseExtender};
use serde_derive::Deserialize;

#[derive(Deserialize, StateData, StaticResponseExtender)]
pub struct GetPathExtractor {
    name: String,
}

pub fn get(state: State) -> (State, impl IntoResponse) {
    let body = {
        let path_params = GetPathExtractor::borrow_from(&state);
        format!("Hello, {}!", &path_params.name)
    };
    (state, body)
}
