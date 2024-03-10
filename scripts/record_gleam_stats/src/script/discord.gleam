import gleam/dynamic
import gleam/hackney
import gleam/http/request
import gleam/json as j
import gleam/result
import script/error.{type Error}

pub fn get_approximate_discord_member_count() -> Result(Int, Error) {
  let assert Ok(request) =
    request.to("https://discord.com/api/v9/invites/Fm8Pwmy?with_counts=true")

  let decoder = dynamic.field("approximate_member_count", of: dynamic.int)

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use members <- result.then(
    response.body
    |> j.decode(using: decoder)
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(members)
}
