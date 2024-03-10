import gleam/hackney
import gleam/http/response.{type Response}
import gleam/json

pub type Error {
  HttpError(hackney.Error)
  UnexpectedJson(json.DecodeError)
  UnexpectedHtml
  UnexpectedHttpStatus(expected: Int, response: Response(String))
  MissingEnvironmentVariable(name: String)
}

pub fn ensure_status(
  response: Response(String),
  is code: Int,
) -> Result(Response(String), Error) {
  case response.status == code {
    True -> Ok(response)
    False -> Error(UnexpectedHttpStatus(expected: code, response: response))
  }
}
