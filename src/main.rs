extern crate lib;

fn main() {
    lib::Server::new().listen("8080")
}
