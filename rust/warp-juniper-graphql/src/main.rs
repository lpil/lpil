#![deny(warnings)]
use warp::http::Response;
use warp::{http::StatusCode, Filter};

fn main() {
    // Show info level logs by default
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "berry=info");
    }
    pretty_env_logger::init();

    let port = std::env::var("RUST_LOG")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3000);

    log::info!("Starting on 0.0.0.0:{}", port);
    run_server(port)
}

fn run_server(port: u16) {
    use warp::path::*;
    use warp::{any, get2};

    let home = get2().and(end()).map(home);

    let show_user = get2()
        .and(path("user"))
        .and(param())
        .and(end())
        .map(show_user);

    let ready_check = get2()
        .and(path("-"))
        .and(path("ready"))
        .and(end())
        .map(ready_check);

    let health_check = get2()
        .and(path("-"))
        .and(path("health"))
        .and(end())
        .map(health_check);

    let not_found = any().map(not_found);

    let routes = home
        .or(show_user)
        .or(health_check)
        .or(ready_check)
        .or(not_found);

    let stack = routes.with(warp::log("berry"));

    warp::serve(stack).run(([0, 0, 0, 0], port));
}

fn home() -> impl warp::Reply {
    "Hello, world!"
}

fn show_user(name: String) -> impl warp::Reply {
    format!("{}'s page", name)
}

fn not_found() -> impl warp::Reply {
    Response::builder().status(404).body("Not found")
}

fn ready_check() -> impl warp::Reply {
    StatusCode::OK
}

fn health_check() -> impl warp::Reply {
    StatusCode::OK
}
