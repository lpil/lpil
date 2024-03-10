import gleam/dynamic as dy
import gleam/hackney
import gleam/http/request
import gleam/json as j
import gleam/result
import script/error.{type Error}

pub fn get_stdlib_counts() -> Result(#(Int, Int), Error) {
  let assert Ok(request) =
    request.to("https://hex.pm/api/packages/gleam_stdlib")

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
