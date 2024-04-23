import carpenter/table.{type Set}
import gleam/bytes_builder
import gleam/otp/actor
import glisten
import redis/resp.{type RespData}

pub type State =
  Set(String, RespData)

pub fn handle_ping(
  state: state,
  conn: glisten.Connection(a),
) -> actor.Next(glisten.Message(a), state) {
  let pong = resp.encode(resp.String("PONG"))
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(pong))
  actor.continue(state)
}

pub fn handle_echo(
  state: state,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), state) {
  let assert [value, ..] = arguments
  let pong = resp.encode(value)
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(pong))
  actor.continue(state)
}

pub fn handle_set(
  table: State,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), State) {
  let assert [resp.String(key), value, ..] = arguments
  table.insert(table, [#(key, value)])
  let response = resp.encode(resp.String("OK"))
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(response))
  actor.continue(table)
}

pub fn handle_get(
  table: State,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), State) {
  let assert [resp.String(key), ..] = arguments

  let response = case table.lookup(table, key) {
    [#(_, value)] -> value
    _ -> resp.Null
  }
  let response = resp.encode(response)
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(response))
  actor.continue(table)
}
