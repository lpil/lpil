import gleam/dynamic/decode
import gleam/hackney
import gleam/http/request
import gleam/json
import gleam/result
import script/error.{type Error}

pub fn get_stdlib_counts() -> Result(#(Int, Int), Error) {
  let assert Ok(request) =
    request.to("https://hex.pm/api/packages/gleam_stdlib")

  let decoder = {
    use all <- decode.subfield(["downloads", "all"], decode.int)
    use recent <- decode.subfield(["downloads", "recent"], decode.int)
    decode.success(#(all, recent))
  }

  use response <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
  )

  use amounts <- result.try(
    response.body
    |> json.parse(decoder)
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(amounts)
}
