use gotham::{handler::IntoResponse, state::State};

pub fn get(state: State) -> (State, impl IntoResponse) {
    (state, "Still alive!")
}
