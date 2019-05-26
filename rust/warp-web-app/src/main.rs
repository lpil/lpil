mod graphql;

use askama::Template;
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

    let stack = routes().with(warp::log("berry"));

    log::info!("Starting on 0.0.0.0:{}", port);
    warp::serve(stack).run(([0, 0, 0, 0], port));
}

fn routes() -> impl warp::Filter<Extract = (impl warp::Reply), Error = warp::Rejection> + Clone {
    use warp::path::*;
    use warp::{any, get2 as get};

    let home = get().and(end()).map(home);

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

    // GET /graphiql/...
    let graphiql_ui = get()
        .and(path("graphiql"))
        .and(end())
        .and(juniper_warp::graphiql_filter("/graphql"));

    // * /graphql
    let graphql_endpoint = path("graphql").and(crate::graphql::filter());

    let not_found = any().map(not_found);

    home.or(health_check)
        .or(ready_check)
        .or(graphiql_ui)
        .or(graphql_endpoint)
        .or(not_found)
}

#[derive(Template)]
#[template(path = "home.html")]
struct HelloTemplate {
    time: String,
}

fn home() -> impl warp::Reply {
    let datetime: chrono::DateTime<chrono::offset::Utc> = std::time::SystemTime::now().into();
    let time = format!("{}", datetime.format("%d/%m/%Y %T"));
    Response::builder()
        .status(200)
        .header("content-type", "text/html")
        .body(HelloTemplate { time }.render().unwrap())
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

// copypasta
