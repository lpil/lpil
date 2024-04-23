import carpenter/table
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import gleam/string
import glisten
import redis/commands.{type State}
import redis/resp

pub fn main() {
  // Start an ETS table, an in-memory key-value store which all the TCP
  // connection handling actors can read and write shares state to and from.
  let assert Ok(ets) =
    table.build("redis")
    |> table.privacy(table.Public)
    |> table.write_concurrency(table.WriteConcurrency)
    |> table.read_concurrency(True)
    |> table.compression(False)
    |> table.set

  // Start the TCP acceptor pool. Each connection will get its own actor to
  // handle Redis requests.
  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(ets, None) }, fn(message, state, conn) {
      case message {
        glisten.User(_) -> actor.continue(state)
        glisten.Packet(data) -> handle_message(state, conn, data)
      }
    })
    |> glisten.serve(6379)

  // Suspend the main process while the acceptor pool works.
  process.sleep_forever()
}

fn handle_message(
  state: State,
  conn: glisten.Connection(a),
  message: BitArray,
) -> actor.Next(glisten.Message(a), State) {
  // TODO: here we assume the full message comes in one and it can be fully
  // parsed with no extra content.
  let assert Ok(resp.Parsed(data, <<>>)) = resp.parse(message)
  let assert resp.Array([resp.String(command), ..arguments]) = data

  case string.lowercase(command) {
    "ping" -> commands.handle_ping(state, conn)
    "echo" -> commands.handle_echo(state, conn, arguments)
    "set" -> commands.handle_set(state, conn, arguments)
    "get" -> commands.handle_get(state, conn, arguments)
    _ -> panic as { "Unknown command: " <> command }
  }
}
