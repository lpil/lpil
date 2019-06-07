use std::net::{TcpListener, TcpStream};
use std::io::Read;
use std::io::Write;

#[derive(Default)]
pub struct Server;

impl Server {
    pub fn new() -> Self {
        Default::default()
    }

    /// Start the HTTP server.
    ///
    pub fn listen(self: Self, port: &str) {
        let address = format!("127.0.0.1:{}", port);
        let listener = TcpListener::bind(address).expect("Unable to bind");

        // accept connections and process them serially
        for stream_result in listener.incoming() {
            let mut stream = stream_result.expect("Unable to handle TCP stream");
            handle_request(stream).expect("Unable to handle request");
        }
    }
}

fn handle_request(mut stream: TcpStream) -> Result<(), ()> {
    let result = stream.read(&mut [0; 1024]);
    println!("{:?}", result);

    let response = String::from("HTTP/1.1 200 OK\n\nHello, World!").into_bytes();
    let _ = stream.write(&response);
    Ok(())
}
