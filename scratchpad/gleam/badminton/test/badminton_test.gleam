import badminton.{
  admin_console, display, editable, email, field, int, references, resource,
  text,
}
import gleam/pgo.{type Connection}
import wisp.{type Request, type Response}

pub fn handle_request(request: Request, db: Connection) -> Response {
  let users =
    resource("users", "Users")
    |> field(text("name") |> editable(True))
    |> field(int("age") |> display("Age (years)") |> editable(True))
    |> field(email("email") |> editable(True))
    |> field(text("payment_reference"))

  let payments =
    resource("payments", "Payments")
    |> field(text("transaction_id"))
    |> field(int("amount"))
    |> references("users", "payment_reference", "name", text("reference"))

  use <- admin_console(request, db, under: "__admin__", for: [users, payments])

  wisp.not_found()
}
