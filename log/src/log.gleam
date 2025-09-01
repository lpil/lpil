import envoy
import wisp

pub fn main() {
  wisp.configure_logger()

  let assert Ok(_secret_key_base) = envoy.get("SECRET_KEY_BASE")
    as "SECRET_KEY_BASE environment variable must be set"
  let assert Ok(_data_path) = envoy.get("DATA_PATH")
    as "DATA_PATH environment variable must be set"
}
