import mist
import gleam/io
import gleam/erlang
import gleam/bit_builder
import gleam/http/response.{Response}

pub fn main() {
  // Start the web server
  assert Ok(_) = mist.run_service(8080, web_service)

  // Put the main actor to sleep while the server works
  erlang.sleep_forever()
}

fn web_service(_request) {
  io.println("Handling a request")
  let body = bit_builder.from_string("Hello, Joe!")
  Response(200, [], body)
}
