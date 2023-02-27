import script/error.{Error}
import script/config.{Config}
import gleam/json as j
import gleam/result
import gleam/dynamic as dy
import gleam/hackney
import gleam/http
import gleam/http/request

pub fn get_estimated_monthly_income_in_cents(
  config: Config,
) -> Result(#(Int, Int), Error) {
  let query =
    "query {
  viewer {
    monthlyEstimatedSponsorsIncomeInCents
    sponsors {
      totalCount
    }
  }
}"

  let query = j.to_string(j.object([#("query", j.string(query))]))

  let request =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host("api.github.com")
    |> request.set_path("/graphql")
    |> request.prepend_header("content-type", "application/json")
    |> request.prepend_header("authorization", "bearer " <> config.github_token)
    |> request.set_body(query)

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use amounts <- result.then(
    response.body
    |> j.decode(using: dy.field(
      "data",
      of: dy.field(
        "viewer",
        of: dy.decode2(
          fn(a, b) { #(a, b) },
          dy.field("monthlyEstimatedSponsorsIncomeInCents", of: dy.int),
          dy.field("sponsors", of: dy.field("totalCount", of: dy.int)),
        ),
      ),
    ))
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(amounts)
}
