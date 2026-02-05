import gleam/dynamic/decode
import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/json
import gleam/result
import script/config.{type Config}
import script/error.{type Error}

pub type Information {
  Information(
    estimated_monthly_sponsorship: Int,
    sponsor_count: Int,
    stars: Int,
  )
}

fn information_decoder() -> decode.Decoder(Information) {
  use estimated_monthly_sponsorship <- decode.subfield(
    ["data", "viewer", "monthlyEstimatedSponsorsIncomeInCents"],
    decode.int,
  )
  use sponsor_count <- decode.subfield(
    ["data", "viewer", "sponsors", "totalCount"],
    decode.int,
  )
  use stars <- decode.subfield(
    ["data", "repositoryOwner", "repository", "stargazerCount"],
    decode.int,
  )
  decode.success(Information(
    estimated_monthly_sponsorship:,
    sponsor_count:,
    stars:,
  ))
}

pub fn get_information(config: Config) -> Result(Information, Error) {
  let query =
    "query {
  viewer {
    monthlyEstimatedSponsorsIncomeInCents
    sponsors {
      totalCount
    }
  }
  repositoryOwner(login: \"gleam-lang\") {
    repository(name: \"gleam\") { stargazerCount }
  }
}"

  let query = json.to_string(json.object([#("query", json.string(query))]))

  let request =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host("api.github.com")
    |> request.set_path("/graphql")
    |> request.prepend_header("content-type", "application/json")
    |> request.prepend_header("authorization", "bearer " <> config.github_token)
    |> request.set_body(query)

  use response <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
  )

  response.body
  |> json.parse(using: information_decoder())
  |> result.map_error(error.UnexpectedJson)
}
