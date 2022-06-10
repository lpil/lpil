import gleam/erlang/os

pub type Config {
  Config(
    // Google sheets
    client_id: String,
    client_secret: String,
    refresh_token: String,
  )
}

pub fn load_from_environment() -> Result(Config, Nil) {
  try client_id = os.get_env("CLIENT_ID")
  try client_secret = os.get_env("CLIENT_SECRET")
  try refresh_token = os.get_env("REFRESH_TOKEN")

  Ok(Config(
    client_id: client_id,
    client_secret: client_secret,
    refresh_token: refresh_token,
  ))
}
