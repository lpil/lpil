import argv
import carpenter/table
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import gleam/string
import glisten
import redis/commands.{type Config, type State, Config, State}
import redis/resp

pub fn main() {
  let config = load_config()

  // Start an ETS table, an in-memory key-value store which all the TCP
  // connection handling actors can read and write shares state to and from.
  let assert Ok(ets) =
    table.build("redis")
    |> table.privacy(table.Public)
    |> table.write_concurrency(table.WriteConcurrency)
    |> table.read_concurrency(True)
    |> table.compression(False)
    |> table.set

  let state = State(table: ets, config: config)

  // Start the TCP acceptor pool. Each connection will get its own actor to
  // handle Redis requests.
  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(state, None) }, fn(message, state, conn) {
      case message {
        glisten.User(_) -> actor.continue(state)
        glisten.Packet(data) -> handle_message(state, conn, data)
      }
    })
    |> glisten.serve(6379)

  // Suspend the main process while the acceptor pool works.
  process.sleep_forever()
}

fn load_config() -> Config {
  let defaults = Config(directory: "/tmp", db_file_name: "dump.rdb")
  parse_argv(defaults, argv.load().arguments)
}

fn parse_argv(config: Config, arguments: List(String)) -> Config {
  case arguments {
    [] -> config
    ["--dir", directory, ..arguments] -> {
      let config = Config(..config, directory: directory)
      parse_argv(config, arguments)
    }
    ["--dbfilename", name, ..arguments] -> {
      let config = Config(..config, db_file_name: name)
      parse_argv(config, arguments)
    }
    _ -> panic as { "Unexpected arguments " <> string.inspect(arguments) }
  }
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
    "config" -> commands.handle_config(state, conn, arguments)
    "ping" -> commands.handle_ping(state, conn)
    "echo" -> commands.handle_echo(state, conn, arguments)
    "set" -> commands.handle_set(state, conn, arguments)
    "get" -> commands.handle_get(state, conn, arguments)
    _ -> panic as { "Unknown command: " <> command }
  }
}
