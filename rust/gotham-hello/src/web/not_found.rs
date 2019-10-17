use gotham::{handler::IntoResponse, state::State};
use hyper::{Response, StatusCode};

pub fn handle(state: State) -> (State, impl IntoResponse) {
    let resp = Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body("There's nothing here...".into())
        .unwrap();
    (state, resp)
}
