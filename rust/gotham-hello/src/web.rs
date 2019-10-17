mod health_check;
mod hello;
mod not_found;

pub fn router() -> gotham::router::Router {
    use gotham::router::{builder::*, route::matcher::any::AnyRouteMatcher};

    build_simple_router(|route| {
        route.get("/health").to(health_check::get);

        route
            .get("/hello/:name")
            .with_path_extractor::<hello::GetPathExtractor>()
            .to(hello::get);

        route
            .request(AnyRouteMatcher::new(), "*")
            .to(not_found::handle);
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use gotham::test::TestServer;
    use hyper::StatusCode;

    #[test]
    fn not_found_test() {
        let response = TestServer::new(router())
            .unwrap()
            .client()
            .get("http://localhost/magic-beans")
            .perform()
            .unwrap();
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        assert_eq!(
            String::from_utf8(response.read_body().unwrap()).expect("Found invalid UTF-8"),
            "There's nothing here..."
        );
    }

    #[test]
    fn hello_test() {
        let response = TestServer::new(router())
            .unwrap()
            .client()
            .get("http://localhost/hello/Louis")
            .perform()
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            String::from_utf8(response.read_body().unwrap()).expect("Found invalid UTF-8"),
            "Hello, Louis!"
        );

        let response = TestServer::new(router())
            .unwrap()
            .client()
            .get("http://localhost/hello/world")
            .perform()
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            String::from_utf8(response.read_body().unwrap()).expect("Found invalid UTF-8"),
            "Hello, world!"
        );
    }

    #[test]
    fn health_check_test() {
        let response = TestServer::new(router())
            .unwrap()
            .client()
            .get("http://localhost/health")
            .perform()
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            String::from_utf8(response.read_body().unwrap()).expect("Found invalid UTF-8"),
            "Still alive!"
        );
    }
}
