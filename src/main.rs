mod graphql;

use warp::http::Response;
use warp::{http::StatusCode, Filter};

fn main() {
    // Show info level logs by default
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "happylabs-graphql=info");
    }
    pretty_env_logger::init();

    let port = std::env::var("PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3000);

    let stack = routes().with(warp::log("happylabs-graphql"));

    log::info!("Starting on 0.0.0.0:{}", port);
    // warp::serve(stack).run(([0, 0, 0, 0], port));
    warp::serve(stack).run(([127, 0, 0, 1], port));
}

fn routes() -> impl warp::Filter<Extract = (impl warp::Reply), Error = warp::Rejection> + Clone {
    use warp::path::*;
    use warp::{any, get2 as get, post2 as post};

    // GET /-/ready
    let ready_check = get()
        .and(path("-"))
        .and(path("ready"))
        .and(end())
        .map(ready_check);

    // GET /-/health
    let health_check = get()
        .and(path("-"))
        .and(path("health"))
        .and(end())
        .map(health_check);

    // GET /
    let graphiql_ui = get()
        .and(end())
        .and(juniper_warp::graphiql_filter("/graphql"));

    // POST /graphql
    let graphql_endpoint = post()
        .and(path("graphql"))
        .and(end())
        .and(crate::graphql::filter());

    let not_found = any().map(not_found);

    graphiql_ui
        .or(health_check)
        .or(ready_check)
        .or(graphql_endpoint)
        .or(not_found)
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

#[test]
fn home_test() {
    let res = warp::test::request()
        .path("/")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}

#[test]
fn not_found_test() {
    let res = warp::test::request()
        .path("/whatever")
        .method("GET")
        .reply(&routes());
    assert_eq!(404, res.status());
}

#[test]
fn health_test() {
    let res = warp::test::request()
        .path("/-/health")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}

#[test]
fn ready_test() {
    let res = warp::test::request()
        .path("/-/ready")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}
