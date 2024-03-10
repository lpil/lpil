import gleam/dynamic as dy
import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/json as j
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

  let decoder =
    dy.decode3(
      Information,
      dy.field(
        "viewer",
        dy.field("monthlyEstimatedSponsorsIncomeInCents", of: dy.int),
      ),
      dy.field(
        "viewer",
        dy.field("sponsors", of: dy.field("totalCount", of: dy.int)),
      ),
      dy.field(
        "repositoryOwner",
        dy.field("repository", of: dy.field("stargazerCount", of: dy.int)),
      ),
    )

  response.body
  |> j.decode(using: dy.field("data", decoder))
  |> result.map_error(error.UnexpectedJson)
}
