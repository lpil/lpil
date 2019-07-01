#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

mod graphql;
mod survey;
mod web;

fn main() {
    use warp::Filter;

    // Show info level logs by default
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "happylabs-graphql=info");
    }
    pretty_env_logger::init();

    let db = crate::survey::Database::new();
    db.seed();

    let port = std::env::var("PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3000);

    let stack = crate::web::routes(db)
        .with(warp::log("happylabs-api-rust"))
        .with(warp::cors().allow_any_origin());

    log::info!("Starting on localhost:{}", port);
    warp::serve(stack).run(([127, 0, 0, 1], port));
}
