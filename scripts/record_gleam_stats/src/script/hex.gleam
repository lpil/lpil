import script/error.{Error}
import gleam/json as j
import gleam/result
import gleam/dynamic as dy
import gleam/hackney
import gleam/http
import gleam/http/request

pub fn get_stdlib_counts() -> Result(#(Int, Int), Error) {
  let request =
    request.new()
    |> request.set_method(http.Get)
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/gleam_stdlib")

  let decoder =
    dy.field(
      "downloads",
      of: dy.decode2(
        fn(a, b) { #(a, b) },
        dy.field("all", of: dy.int),
        dy.field("recent", of: dy.int),
      ),
    )

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use amounts <- result.then(
    response.body
    |> j.decode(using: decoder)
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(amounts)
}
