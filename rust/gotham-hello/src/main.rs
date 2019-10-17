mod web;

use tracing::info;

fn main() {
    init_logger().expect("Could not init logger");
    start_web_server();
}

fn init_logger() -> Result<(), impl std::error::Error> {
    let stdout_subscriber = tracing_subscriber::fmt::Subscriber::builder()
        .with_writer(std::io::stdout)
        .finish();
    tracing::subscriber::set_global_default(stdout_subscriber)
}

fn start_web_server() {
    let port = std::env::var("PORT")
        .ok()
        .unwrap_or_else(|| "3000".to_string());

    info!(port = &*port, "Starting web server!");
    gotham::start(format!("127.0.0.1:{}", port), web::router())
}
