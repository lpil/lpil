import gleam/dynamic/decode
import gleam/hackney
import gleam/http/request
import gleam/json
import gleam/result
import script/config.{type Config}
import script/error.{type Error}

pub type Information {
  Information(thirty_day_visitors: Int, thirty_day_pageviews: Int)
}

fn information_decoder() -> decode.Decoder(Information) {
  use thirty_day_visitors <- decode.subfield(
    ["results", "visitors", "value"],
    decode.int,
  )
  use thirty_day_pageviews <- decode.subfield(
    ["results", "pageviews", "value"],
    decode.int,
  )
  decode.success(Information(thirty_day_visitors:, thirty_day_pageviews:))
}

pub fn get_stats(config: Config) -> Result(Information, Error) {
  let assert Ok(request) =
    request.to(
      "https://plausible.io/api/v1/stats/aggregate?site_id=gleam.run&period=30d&metrics=visitors,pageviews",
    )

  let request =
    request
    |> request.prepend_header(
      "authorization",
      "Bearer " <> config.plausible_token,
    )

  use response <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
  )

  use members <- result.try(
    response.body
    |> json.parse(using: information_decoder())
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(members)
}
