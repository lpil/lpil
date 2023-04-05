import script/error.{Error}
import gleam/string
import gleam/result
import gleam/hackney
import gleam/list
import gleam/http
import gleam/http/request
import gleam/int
import html_parser.{Characters, EndElement, StartElement}

pub type Information {
  Information(
    students_count: Int,
    submissions_count: Int,
    mentoring_discussions_count: Int,
  )
}

pub fn get_track_information() -> Result(Information, Error) {
  let request =
    request.new()
    |> request.set_method(http.Get)
    |> request.set_host("exercism.org")
    |> request.set_path("/tracks/gleam/build")

  use response <- result.then(
    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200)),
  )

  use info <- result.then(parse_track_page(response.body))

  Ok(info)
}

type ParseState {
  Inside(depth: Int, content: List(String), previous: List(List(String)))
  Outside(content: List(List(String)))
}

pub fn parse_track_page(html: String) -> Result(Information, Error) {
  use state <- result.then(
    html_parser.sax(html, Outside([]), handle_event)
    |> result.replace_error(error.UnexpectedHtml),
  )

  use #(discussions, submissions, students) <- result.then(case state {
    Outside([["Mentoring Discussions", a], ["Submissions", b], ["Students", c]]) ->
      Ok(#(a, b, c))
    _ -> Error(error.UnexpectedHtml)
  })

  use students_count <- result.then(parse_int(students))
  use submissions_count <- result.then(parse_int(submissions))
  use mentoring_discussions_count <- result.then(parse_int(discussions))

  Ok(Information(
    students_count: students_count,
    submissions_count: submissions_count,
    mentoring_discussions_count: mentoring_discussions_count,
  ))
}

fn handle_event(
  state: ParseState,
  _line: Int,
  event: html_parser.SaxEvent,
) -> ParseState {
  case state, event {
    Outside(previous), StartElement(attributes: attributes, ..) ->
      case has_class(attributes, "report-stat") {
        True -> Inside(1, [], previous)
        False -> state
      }

    Inside(depth, content, previous), Characters(c) if c != "" ->
      Inside(depth, [c, ..content], previous)

    Inside(depth, content, previous), StartElement(..) ->
      Inside(depth + 1, content, previous)

    Inside(1, content, previous), EndElement(..) ->
      Outside([list.reverse(content), ..previous])

    Inside(depth, content, previous), EndElement(..) ->
      Inside(depth - 1, content, previous)

    _, _ -> state
  }
}

fn has_class(attributes: List(html_parser.Attribute), class: String) -> Bool {
  case attributes {
    [html_parser.Attribute(name: "class", value: value, ..), ..] if value == class ->
      True
    [_, ..rest] -> has_class(rest, class)
    [] -> False
  }
}

fn parse_int(string: String) -> Result(Int, Error) {
  string
  |> string.replace(",", "")
  |> int.parse
  |> result.replace_error(error.UnexpectedHtml)
}