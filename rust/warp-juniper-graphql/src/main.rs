use warp::http::Response;
use warp::Filter;

fn main() {
    use warp::path::*;
    use warp::{any, get2};

    pretty_env_logger::init();
    let log = warp::log("berry");

    let routes = get2()
        .and(end())
        .map(home)
        .or(get2().and(path("user")).and(param()).and(end()).map(user))
        .or(any().map(not_found));

    warp::serve(routes.with(log)).run(([0, 0, 0, 0], 3000));
}

fn home() -> impl warp::Reply {
    "Hello, world!"
}

fn user(name: String) -> impl warp::Reply {
    format!("{}'s page", name)
}

fn not_found() -> impl warp::Reply {
    Response::builder().status(404).body("Not found")
}
