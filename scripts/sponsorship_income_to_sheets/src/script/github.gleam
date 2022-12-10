import script/error.{Error}
import script/config.{Config}
import gleam/json as j
import gleam/result
import gleam/dynamic
import gleam/hackney
import gleam/http
import gleam/http/request

pub fn get_estimated_monthly_income_in_cents(
  config: Config,
) -> Result(Int, Error) {
  let query =
    j.to_string(j.object([
      #(
        "query",
        j.string("query { viewer { monthlyEstimatedSponsorsIncomeInCents } }"),
      ),
    ]))

  let request =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host("api.github.com")
    |> request.set_path("/graphql")
    |> request.prepend_header("content-type", "application/json")
    |> request.prepend_header("authorization", "bearer " <> config.github_token)
    |> request.set_body(query)

  try response =
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200))

  try cents =
    response.body
    |> j.decode(using: dynamic.field(
      "data",
      of: dynamic.field(
        "viewer",
        of: dynamic.field(
          "monthlyEstimatedSponsorsIncomeInCents",
          of: dynamic.int,
        ),
      ),
    ))
    |> result.map_error(error.UnexpectedJson)

  Ok(cents)
}
