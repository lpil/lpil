import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/time/timestamp
import teletext/error.{type TeletextError}

pub type Event {
  Event(time: timestamp.Timestamp, topic: String, payload: String)
}

pub fn parse(input: String) -> Result(List(Event), Nil) {
  let rows =
    string.split(input, "\n")
    |> list.map(string.split(_, "\t"))
    |> list.filter(fn(line) { line != [""] })

  list.try_map(rows, se_row(co: List(String)) -> Result(Event, Nil) {
  use #(unix_ms, topic, payload) <- result.try(case columns {
    [timestamp, topic, payload] -> Ok(#(timestamp, topic, payload))
    _ -> Error(Nil)
  })

  use unix_ms <- result.try(int.parse(unix_ms))
  let time = timestamp.from_unix_seconds(unix_ms / 1000)
  Ok(Event(time:, topic:, payload:))
}
