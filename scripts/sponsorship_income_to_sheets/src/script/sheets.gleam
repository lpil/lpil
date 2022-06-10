import gleam/dynamic
import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/http/response.{Response}
import gleam/int
import gleam/json.{Json} as j
import gleam/result
import gleam/string
import script/config.{Config}

// https://docs.google.com/spreadsheets/d/1OLaTgpN9MXTVNZ--6s6AjE5aCjfpm2lAuy6FUmPuGRI/edit#gid=0
const sheet_id = "1OLaTgpN9MXTVNZ--6s6AjE5aCjfpm2lAuy6FUmPuGRI"

const sheet_name = "monthly-sponsorship"

pub type Error {
  HttpError(hackney.Error)
  UnexpectedJson(j.DecodeError)
  UnexpectedHttpStatus(expected: Int, response: Response(String))
}

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

  try response =
    hackney.send(request)
    |> result.map_error(HttpError)
    |> result.then(ensure_status(_, is: 200))

  try json =
    response.body
    |> j.decode(using: dynamic.field("access_token", of: dynamic.string))
    |> result.map_error(UnexpectedJson)

  Ok(json)
}

fn ensure_status(
  response: Response(String),
  is code: Int,
) -> Result(Response(String), Error) {
  case response.status == code {
    True -> Ok(response)
    False -> Error(UnexpectedHttpStatus(expected: code, response: response))
  }
}

fn append_row(
  sheet: String,
  row: List(Json),
  config: Config,
) -> Result(Nil, Error) {
  try access_token = get_access_token(config)

  let json =
    j.to_string(j.object([
      #("range", j.string(string.append(sheet, "!A:A"))),
      #("majorDimension", j.string("ROWS")),
      #("values", j.preprocessed_array([j.preprocessed_array(row)])),
    ]))

  let path =
    string.concat([
      "/v4/spreadsheets/",
      sheet_id,
      "/values/",
      sheet_name,
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

  try _ =
    hackney.send(request)
    |> result.map_error(HttpError)
    |> result.then(ensure_status(_, is: 200))

  Ok(Nil)
}

pub fn append_current_income(cents: Int, config: Config) -> Result(Nil, Error) {
  let money =
    string.concat([
      "$",
      int.to_string(cents / 100),
      ".",
      string.pad_left(int.to_string(cents % 100), to: 2, with: "0"),
    ])
  let row = [j.string(timestamp()), j.string(money)]
  append_row("attendees", row, config)
}

external fn timestamp() -> String =
  "script_ffi" "timestamp"
