import gleam/dict
import gleam/list
import gleam/string
import gloss/environment

pub fn variables_test() {
  // All
  let all = environment.all()
  let assert Ok(_) = dict.get(all, "LANG")
  assert dict.get(all, "UAGE") == Error(Nil)

  // Get, set
  let assert Error(_) = environment.get("UAGE")
  environment.set("UAGE", "anglish")
  assert environment.get("UAGE") == Ok("anglish")
  let all = environment.all()
  assert dict.get(all, "UAGE") == Ok("anglish")
  // Unset
  environment.unset("UAGE")
  assert environment.get("UAGE") == Error(Nil)
  let all = environment.all()
  assert dict.get(all, "UAGE") == Error(Nil)
  // Unicode
  environment.set("WIBBLE", "🦄")
  assert environment.get("WIBBLE") == Ok("🦄")
  let all = environment.all()
  assert dict.get(all, "WIBBLE") == Ok("🦄")
}

pub fn system_name_test() {
  assert ["darwin", "freebsd", "linux", "openbsd", "win32"]
    |> list.contains(environment.system_name())
}

pub fn home_directory_test() {
  let assert Ok(path) = environment.home_directory()
  let separator = case environment.system_name() {
    "win32" -> "\\"
    _ -> "/"
  }
  assert string.contains(path, separator)
}

pub fn temporary_directory_test() {
  let path = environment.temporary_directory()
  let separator = case environment.system_name() {
    "win32" -> "\\"
    _ -> "/"
  }
  assert string.contains(path, separator)
}
