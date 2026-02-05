import envoy
import gleam/result
import script/error.{type Error}

pub type Config {
  Config(
    // Google sheets
    client_id: String,
    client_secret: String,
    refresh_token: String,
    // GitHub
    github_token: String,
    // Plausible
    plausible_token: String,
  )
}

pub fn load_from_environment() -> Result(Config, Error) {
  use client_id <- result.try(env("GCP_CLIENT_ID"))
  use client_secret <- result.try(env("GCP_CLIENT_SECRET"))
  use refresh_token <- result.try(env("GCP_REFRESH_TOKEN"))
  use github_token <- result.try(env("GITHUB_TOKEN"))
  use plausible_token <- result.try(env("PLAUSIBLE_TOKEN"))

  Ok(Config(
    client_id: client_id,
    client_secret: client_secret,
    refresh_token: refresh_token,
    github_token: github_token,
    plausible_token: plausible_token,
  ))
}

fn env(name: String) -> Result(String, Error) {
  envoy.get(name)
  |> result.replace_error(error.MissingEnvironmentVariable(name))
}
