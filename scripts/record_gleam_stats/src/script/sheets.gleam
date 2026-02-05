import gleam/dynamic/decode
import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/int
import gleam/json.{type Json}
import gleam/result
import gleam/string
import script/config.{type Config}
import script/error.{type Error}

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

  use response <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
  )

  use json <- result.try(
    response.body
    |> json.parse(decode.at(["access_token"], decode.string))
    |> result.map_error(error.UnexpectedJson),
  )

  Ok(json)
}

fn append_row(
  sheet: String,
  row: List(Json),
  config: Config,
) -> Result(Nil, Error) {
  use access_token <- result.try(get_access_token(config))

  let json =
    json.to_string(
      json.object([
        #("range", json.string(sheet <> "!A:A")),
        #("majorDimension", json.string("ROWS")),
        #("values", json.preprocessed_array([json.preprocessed_array(row)])),
      ]),
    )

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

  use _ <- result.try(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200)),
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
    json.string(timestamp()),
    json.string(cents_to_dollars(row.monthly_sponsorship_cents)),
    json.int(row.sponsor_count),
    json.int(row.compiler_github_stars),
    json.int(row.approximate_discord_member_count),
    json.int(row.stdlib_all_downloads),
    json.int(row.stdlib_recent_downloads),
    json.int(row.site_thirty_day_visitors),
    json.int(row.site_thirty_day_pageviews),
    json.int(row.exercism_students_count),
    json.int(row.exercism_submissions_count),
    json.int(row.exercism_mentoring_discussions_count),
  ]
  append_row(sheet_name, row, config)
}

fn cents_to_dollars(cents: Int) -> String {
  int.to_string(cents / 100)
  <> "."
  <> string.pad_start(int.to_string(cents % 100), to: 2, with: "0")
}

@external(erlang, "script_ffi", "timestamp")
fn timestamp() -> String
