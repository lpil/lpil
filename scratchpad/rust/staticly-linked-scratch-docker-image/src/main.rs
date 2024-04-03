use actix_web::{middleware, web, App, HttpRequest, HttpServer};

fn index(_req: HttpRequest) -> &'static str {
    "Hello world!"
}

fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "actix_web=info");
    env_logger::init();

    println!("Starting on 0.0.0.0:3000");

    HttpServer::new(|| {
        App::new()
            .wrap(middleware::Logger::new(r#"%r %s %b %Dms"#))
            .service(web::resource("/").to(index))
    })
    .bind("0.0.0.0:3000")?
    .run()
}
