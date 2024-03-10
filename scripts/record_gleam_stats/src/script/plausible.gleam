import gleam/dynamic as dy
import gleam/hackney
import gleam/http/request
import gleam/json as j
import gleam/result
import script/config.{type Config}
import script/error.{type Error}

pub type Information {
  Information(thirty_day_visitors: Int, thirty_day_pageviews: Int)
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

  let decoder =
    dy.decode2(
      Information,
      dy.field("visitors", dy.field("value", dy.int)),
      dy.field("pageviews", dy.field("value", dy.int)),
    )

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use members <- result.then(
    response.body
    |> j.decode(using: dy.field("results", decoder))
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(members)
}
