import gleam/list
import gleam/string
import gleam/httpc.{None}
import gleam/http.{Get}

pub external type OkAtom

pub external fn debug_print(anything) -> OkAtom =
  "erlang" "display"

pub external fn print(String) -> OkAtom =
  "io" "fwrite"

pub type Application {
  Clippy
}

pub external fn start_application_and_deps(Application) -> OkAtom =
  "application" "ensure_all_started"

pub fn main(args: List(String)) {
  debug_print(start_application_and_deps(Clippy))

  let result = httpc.request(
    method: Get,
    url: "https://api.ipify.org?format=json",
    headers: [],
    body: None,
  )

  case result {
    Ok(response) -> print(response.body)
    Error(e) -> {
      debug_print(e)
      print("There was an error :(\n")
    }
  }

  print("\n")
}
