import gleam/dynamic
import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/int
import gleam/json.{Json} as j
import gleam/result
import gleam/string
import script/config.{Config}
import script/error.{Error}

// https://docs.google.com/spreadsheets/d/1OLaTgpN9MXTVNZ--6s6AjE5aCjfpm2lAuy6FUmPuGRI/edit#gid=0
const sheet_id = "1OLaTgpN9MXTVNZ--6s6AjE5aCjfpm2lAuy6FUmPuGRI"

const sheet_name = "data"

pub fn get_access_token(config: Config) -> Result(String, Error) {
  let formdata =
    string.concat([
      "client_id=",
      config.client_id,
      "&client_secret=",
      config.client_secret,
      "&refresh_token=",
      config.refresh_token,
      "&grant_type=refresh_token",
    ])

  let request =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host("oauth2.googleapis.com")
    |> request.set_path("/token")
    |> request.prepend_header(
      "content-type",
      "application/x-www-form-urlencoded",
    )
    |> request.set_body(formdata)

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use json <- result.then(
    response.body
    |> j.decode(using: dynamic.field("access_token", of: dynamic.string))
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(json)
}

fn append_row(
  sheet: String,
  row: List(Json),
  config: Config,
) -> Result(Nil, Error) {
  use access_token <- result.then(get_access_token(config))

  let json =
    j.to_string(j.object([
      #("range", j.string(sheet <> "!A:A")),
      #("majorDimension", j.string("ROWS")),
      #("values", j.preprocessed_array([j.preprocessed_array(row)])),
    ]))

  let path =
    string.concat([
      "/v4/spreadsheets/",
      sheet_id,
      "/values/",
      sheet,
      "!A:A:append?valueInputOption=USER_ENTERED&access_token=",
      access_token,
    ])

  let request =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_body(json)
    |> request.set_host("sheets.googleapis.com")
    |> request.set_path(path)
    |> request.prepend_header("content-type", "application/json")

  use _ <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  Ok(Nil)
}

pub type Row {
  Row(
    monthly_sponsorship_cents: Int,
    sponsor_count: Int,
    compiler_github_stars: Int,
    approximate_discord_member_count: Int,
    stdlib_all_downloads: Int,
    stdlib_recent_downloads: Int,
    exercism_students_count: Int,
    exercism_submissions_count: Int,
    exercism_mentoring_discussions_count: Int,
    site_thirty_day_visitors: Int,
    site_thirty_day_pageviews: Int,
  )
}

pub fn append_current_income(row: Row, config: Config) -> Result(Nil, Error) {
  let row = [
    j.string(timestamp()),
    j.string(cents_to_dollars(row.monthly_sponsorship_cents)),
    j.int(row.sponsor_count),
    j.int(row.compiler_github_stars),
    j.int(row.approximate_discord_member_count),
    j.int(row.stdlib_all_downloads),
    j.int(row.stdlib_recent_downloads),
    j.int(row.site_thirty_day_visitors),
    j.int(row.site_thirty_day_pageviews),
    j.int(row.exercism_students_count),
    j.int(row.exercism_submissions_count),
    j.int(row.exercism_mentoring_discussions_count),
  ]
  append_row(sheet_name, row, config)
}

fn cents_to_dollars(cents: Int) -> String {
  int.to_string(cents / 100) <> "." <> string.pad_left(
    int.to_string(cents % 100),
    to: 2,
    with: "0",
  )
}

external fn timestamp() -> String =
  "script_ffi" "timestamp"
