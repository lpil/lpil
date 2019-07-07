use warp::http::StatusCode;

pub fn readiness() -> impl warp::Reply {
    StatusCode::OK
}

pub fn health() -> impl warp::Reply {
    StatusCode::OK
}
