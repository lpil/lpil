import gleam/dict
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
