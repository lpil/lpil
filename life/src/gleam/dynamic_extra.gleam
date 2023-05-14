import gleam/dynamic.{Decoder, Dynamic} as dyn
import gleam/list

pub fn decoder(value: a) -> Decoder(a) {
  fn(_) { Ok(value) }
}

pub fn with(previous: Decoder(fn(a) -> b), next: Decoder(a)) -> Decoder(b) {
  fn(value) {
    case previous(value), next(value) {
      Ok(f), Ok(a) -> Ok(f(a))
      Ok(_), Error(e) -> Error(e)
      Error(e), Ok(_) -> Error(e)
      Error(e1), Error(e2) -> Error(list.append(e1, e2))
    }
  }
}

pub fn run(decoder: Decoder(a), value: Dynamic) -> Result(a, dyn.DecodeErrors) {
  decoder(value)
}
