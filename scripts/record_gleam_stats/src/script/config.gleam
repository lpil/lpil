import gleam/erlang/os
import gleam/result
import script/error.{Error}

pub type Config {
  Config(
    // Google sheets
    client_id: String,
    client_secret: String,
    refresh_token: String,
    // GitHub
    github_token: String,
  )
}

pub fn load_from_environment() -> Result(Config, Error) {
  use client_id <- result.then(env("GCP_CLIENT_ID"))
  use client_secret <- result.then(env("GCP_CLIENT_SECRET"))
  use refresh_token <- result.then(env("GCP_REFRESH_TOKEN"))
  use github_token <- result.then(env("GITHUB_TOKEN"))

  Ok(Config(
    client_id: client_id,
    client_secret: client_secret,
    refresh_token: refresh_token,
    github_token: github_token,
  ))
}

fn env(name: String) -> Result(String, Error) {
  os.get_env(name)
  |> result.replace_error(error.MissingEnvironmentVariable(name))
}
