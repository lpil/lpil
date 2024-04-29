import carpenter/table.{type Set as EtsTable}
import gleam/bytes_builder
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import glisten
import redis/resp.{type RespData}

pub type State {
  State(table: EtsTable(String, Stored), config: Config)
}

pub type Config {
  Config(directory: String, db_file_name: String)
}

pub type Stored {
  Stored(value: RespData, expiry_time: Option(Int))
}

@external(erlang, "os", "timestamp")
fn get_erlang_timestamp() -> #(Int, Int, Int)

fn current_time_ms() -> Int {
  let #(megaseconds, seconds, microseconds) = get_erlang_timestamp()
  let milliseconds = microseconds / 1000
  let seconds = megaseconds * 1_000_000 + seconds
  seconds * 1000 + milliseconds
}

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
  state: State,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), State) {
  let assert [resp.String(key), value, ..arguments] = arguments

  // Pull of the PX argument if there is one: a number of milliseconds until
  // the value should expire.
  let expiry_time = case arguments {
    [resp.String("PX"), resp.String(t)] | [resp.String("px"), resp.String(t)] -> {
      let assert Ok(t) = int.parse(t)
      Some(t + current_time_ms())
    }
    _ -> None
  }

  // Store the value (and expiry time if there is one) in the ETS table
  let stored = Stored(value: value, expiry_time: expiry_time)
  table.insert(state.table, [#(key, stored)])

  let response = resp.encode(resp.String("OK"))
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(response))
  actor.continue(state)
}

pub fn handle_get(
  state: State,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), State) {
  let assert [resp.String(key), ..] = arguments

  let response = resp.encode(get_value(state.table, key))
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(response))
  actor.continue(state)
}

fn get_value(table: EtsTable(String, Stored), key: String) -> RespData {
  case table.lookup(table, key) {
    // If there is no expiry time then we can just return the value
    [#(_, Stored(value, expiry_time: None))] -> value

    // If there is a an expiry time then we need check if it has expired
    [#(_, Stored(value, expiry_time: Some(t)))] ->
      case t < current_time_ms() {
        // Expired! Remove the value
        True -> {
          table.delete(table, key)
          resp.NullString
        }
        // Not Expired
        False -> value
      }

    // There is no value for this key
    _ -> resp.NullString
  }
}

pub fn handle_config(
  state: State,
  conn: glisten.Connection(a),
  arguments: List(RespData),
) -> actor.Next(glisten.Message(a), State) {
  let resp = case arguments {
    [resp.String("get"), resp.String("dir")] ->
      resp.Array([resp.String("dir"), resp.String(state.config.directory)])

    [resp.String("get"), resp.String("dbfilename")] ->
      resp.Array([
        resp.String("dbfilename"),
        resp.String(state.config.db_file_name),
      ])

    _ -> resp.Array([])
  }
  let response = resp.encode(resp)
  let assert Ok(_) = glisten.send(conn, bytes_builder.from_bit_array(response))
  actor.continue(state)
}
