import gap
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam_community/ansi
import snag.{type Snag}

pub opaque type Verse(t) {
  Verse(run: fn(State) -> #(State, Result(Outcome(t), Snag)))
}

pub type State {
  State(dry_run: Bool, changed: Bool, stack: List(String))
}

pub type Outcome(t) {
  Outcome(changed: Bool, data: t)
}

pub fn run(verse: Verse(t)) -> Result(Nil, Snag) {
  let state = State(dry_run: False, changed: False, stack: [])
  verse.run(state).1 |> result.replace(Nil)
}

pub fn print(message: String, rest: fn() -> Verse(t)) -> Verse(t) {
  use state <- Verse
  io.println(message)
  rest().run(state)
}

pub fn print_difference(
  before old: String,
  after new: String,
  rest rest: fn() -> Verse(t),
) -> Verse(t) {
  use state <- Verse
  let diff = gap.compare_strings(old, new) |> gap.to_styled
  io.println(diff.first)
  io.println(diff.second)
  rest().run(state)
}

pub fn include(
  verse: Verse(t1),
  rest: fn(Outcome(t1)) -> Verse(t2),
) -> Verse(t2) {
  use state <- Verse
  let #(state, result) = verse.run(state)
  case result {
    Ok(outcome) -> rest(outcome).run(state)
    Error(e) -> #(state, Error(e))
  }
}

pub fn conditionally_include(
  condition: Bool,
  verse: Verse(t1),
  rest: fn(Outcome(Nil)) -> Verse(t2),
) -> Verse(t2) {
  use state <- Verse
  let #(state, result) = case condition {
    True -> {
      let #(state, result) = verse.run(state)
      let result = case result {
        Ok(o) -> Ok(Outcome(o.changed, data: Nil))
        Error(e) -> Error(e)
      }
      #(state, result)
    }
    False -> #(state, Ok(Outcome(changed: False, data: Nil)))
  }
  case result {
    Ok(outcome) -> rest(outcome).run(state)
    Error(e) -> #(state, Error(e))
  }
}

pub fn finish(data data: t) -> Verse(t) {
  use state <- Verse
  #(state, Ok(Outcome(changed: state.changed, data:)))
}

pub fn include_if_changed(
  outcome: Outcome(anything),
  verse: Verse(t1),
  rest: fn(Outcome(Nil)) -> Verse(t2),
) -> Verse(t2) {
  conditionally_include(outcome.changed, verse, rest)
}

pub fn changed(rest: fn() -> Verse(t1)) -> Verse(t1) {
  use state <- Verse
  let state = State(..state, changed: True)
  rest().run(state)
}

pub fn try(result: Result(t1, Snag), rest: fn(t1) -> Verse(t2)) -> Verse(t2) {
  use state <- Verse
  case result {
    Ok(t) -> rest(t).run(state)
    Error(e) -> #(state, Error(e))
  }
}

pub fn start(name: String, rest: fn() -> Verse(t1)) -> Verse(t1) {
  use initial_state <- Verse

  io.println(
    ansi.grey(case initial_state.stack {
      [] -> name
      x -> string.repeat("  ", list.length(x) - 1) <> "╰ " <> name
    }),
  )

  let state =
    State(..initial_state, changed: False, stack: [name, ..initial_state.stack])
  let #(state, result) = rest().run(state)

  case state.changed {
    False -> Nil
    True ->
      ansi.yellow(
        string.repeat("  ", list.length(initial_state.stack)) <> "changed",
      )
      |> io.println
  }

  let state =
    State(
      ..state,
      stack: initial_state.stack,
      changed: initial_state.changed || state.changed,
    )
  #(state, result)
}
