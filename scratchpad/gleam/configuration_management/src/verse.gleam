import gap
import gleam/io
import snag.{type Snag}

pub opaque type Verse(t) {
  Verse(run: fn(Config) -> #(Config, Result(Outcome(t), Snag)))
}

pub type Config {
  Config(dry_run: Bool)
}

pub type Outcome(t) {
  Outcome(changed: Bool, data: t)
}

pub fn announce(message: String, rest: fn() -> Verse(t)) -> Verse(t) {
  use config <- Verse
  io.println(message)
  rest().run(config)
}

pub fn announce_difference(
  before old: String,
  after new: String,
  rest rest: fn() -> Verse(t),
) -> Verse(t) {
  use config <- Verse
  let diff = gap.compare_strings(old, new) |> gap.to_styled
  io.println(diff.first)
  io.println(diff.second)
  rest().run(config)
}

pub fn include(
  verse: Verse(t1),
  rest: fn(Outcome(t1)) -> Verse(t2),
) -> Verse(t2) {
  use config <- Verse
  let #(config, result) = verse.run(config)
  case result {
    Ok(outcome) -> rest(outcome).run(config)
    Error(e) -> #(config, Error(e))
  }
}

pub fn conditionally_include(
  condition: Bool,
  verse: Verse(t1),
  rest: fn(Outcome(Nil)) -> Verse(t2),
) -> Verse(t2) {
  use config <- Verse
  let #(config, result) = case condition {
    True -> {
      let #(config, result) = verse.run(config)
      let result = case result {
        Ok(o) -> Ok(Outcome(o.changed, data: Nil))
        Error(e) -> Error(e)
      }
      #(config, result)
    }
    False -> #(config, Ok(Outcome(changed: False, data: Nil)))
  }
  case result {
    Ok(outcome) -> rest(outcome).run(config)
    Error(e) -> #(config, Error(e))
  }
}

pub fn include_if_changed(
  outcome: Outcome(anything),
  verse: Verse(t1),
  rest: fn(Outcome(Nil)) -> Verse(t2),
) -> Verse(t2) {
  conditionally_include(outcome.changed, verse, rest)
}
