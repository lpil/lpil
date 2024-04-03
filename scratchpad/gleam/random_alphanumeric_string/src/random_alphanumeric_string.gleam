//// Random strings for HarryET

import gleam/io
import gleam/int
import gleam/list
import gleam/bit_string

pub fn main() {
  use _ <- list.each(list.range(1, 20))
  io.println(random_string())
}

fn random_string() {
  let assert Ok(s) =
    bit_string.to_string(<<
      random_lowercase_ascii_alphanumeric(),
      random_lowercase_ascii_alphanumeric(),
      random_lowercase_ascii_alphanumeric(),
      random_lowercase_ascii_alphanumeric(),
      random_lowercase_ascii_alphanumeric(),
      random_lowercase_ascii_alphanumeric(),
    >>)
  s
}

fn random_lowercase_ascii_alphanumeric() {
  case int.random(48, 84) {
    n if n < 58 -> n
    n -> n + 39
  }
}
