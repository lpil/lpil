import gleam/io
import gleam/erlang/os

pub type Config {
  Config(
    starling_pat: String,
    starling_account_uid: String,
    starling_category_uid: String,
  )
}

fn environment_variable(name: String) -> String {
  case os.get_env(name) {
    Ok(value) -> value
    Error(_) -> {
      io.println("Missing environment variable: " <> name)
      panic
    }
  }
}

pub fn load_from_environment() -> Config {
  Config(
    starling_pat: environment_variable("STARLING_PAT"),
    starling_account_uid: environment_variable("STARLING_ACCOUNT_UID"),
    starling_category_uid: environment_variable("STARLING_CATEGORY_UID"),
  )
}
