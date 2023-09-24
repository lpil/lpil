import gleam/hackney
import gleam/http/request
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import htmgrrrl.{Characters, EndElement, StartElement}
import retry
import script/error.{Error}

pub type Information {
  Information(
    students_count: Int,
    submissions_count: Int,
    mentoring_discussions_count: Int,
  )
}

pub fn get_track_information() -> Result(Information, Error) {
  let assert Ok(request) = request.to("https://exercism.org/tracks/gleam/build")
  let request =
    request
    |> request.set_header("accept", "text/html")
    |> request.set_header("user-agent", "curl/7.85.0")

  use response <- result.then({
    use attempt <- retry.with_retries(backoff: 1000, attempts: 3)
    case attempt {
      0 -> Nil
      _ -> io.println("  Retrying...")
    }

    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.then(error.ensure_status(_, is: 200))
  })

  use info <- result.then(parse_track_page(response.body))

  Ok(info)
}

type ParseState {
  Inside(depth: Int, content: List(String), previous: List(List(String)))
  Outside(content: List(List(String)))
}

pub fn parse_track_page(html: String) -> Result(Information, Error) {
  use state <- result.then(
    htmgrrrl.sax(html, Outside([]), handle_event)
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
  event: htmgrrrl.SaxEvent,
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

fn has_class(attributes: List(htmgrrrl.Attribute), class: String) -> Bool {
  case attributes {
    [htmgrrrl.Attribute(name: "class", value: value, ..), ..] if value == class ->
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
