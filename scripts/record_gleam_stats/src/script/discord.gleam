import gleam/dynamic/decode
import gleam/hackney
import gleam/http/request
import gleam/json
import gleam/result
import script/error.{type Error}

pub fn get_approximate_discord_member_count() -> Result(Int, Error) {
  let assert Ok(request) =
    request.to("https://discord.com/api/v9/invites/Fm8Pwmy?with_counts=true")

  let decoder = decode.at(["approximate_member_count"], decode.int)

  use response <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
  )

  use members <- result.try(
    response.body
    |> json.parse(using: decoder)
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(members)
}
