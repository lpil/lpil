use actix_web::{web, App, HttpServer, Responder};

fn index(info: web::Path<(u32, String)>) -> impl Responder {
    format!("Hello {}! id:{}", info.1, info.0)
}

fn broken(info: web::Path<(u32, String)>) -> impl Responder {
    format!("Hello {}! id:{}", info.1, info.0)
}

fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .service(web::resource("/one/{id}/{name}/index.html").to(index))
            .service(web::resource("/two/{name}/index.html").to(broken)) // This error is not found at compile time. Boo.
    })
    .bind("0.0.0.0:3000")?
    .run()
}
