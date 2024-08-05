import argv
import badminton.{
  admin_console, display, editable, email, field, int, references, resource,
  searchable, text,
}
import gleam/erlang/process
import gleam/http/request
import gleam/pgo.{type Connection}
import gleeunit
import mist
import sql_pgo
import wisp.{type Request, type Response}

pub fn main() {
  case argv.load().arguments {
    ["server"] -> run_server()
    _ -> gleeunit.main()
  }
}

fn run_server() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let db =
    pgo.connect(
      pgo.Config(..pgo.default_config(), database: "gleam_badminton_test"),
    )

  let assert Ok(_) =
    wisp.mist_handler(handle_request(_, db), secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

pub fn handle_request(request: Request, db: Connection) -> Response {
  let request = wisp.method_override(request)

  let users =
    resource("users", "Users")
    |> field(text("name") |> editable(True) |> searchable(True))
    |> field(int("age") |> display("Age (years)") |> editable(True))
    |> field(email("email") |> editable(True) |> searchable(True))
    |> field(text("payment_reference") |> searchable(True))

  let payments =
    resource("payments", "Payments")
    |> field(text("transaction_id"))
    |> field(int("amount"))
    |> references("users", "payment_reference", "name", text("reference"))

  case request.path_segments(request) {
    ["__admin__", ..path] ->
      admin_console(
        request,
        path:,
        execute_query: sql_pgo.make_executor(db),
        for: [users, payments],
      )
    _ -> wisp.not_found()
  }
}
