import gleam/dict
import gleam/hackney
import gleam/http/request
import gleam/int
import gleam/io
import gleam/pair
import gleam/result
import gleam/string
import presentable_soup as soup
import retry
import script/error.{type Error}

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

  use response <- result.try({
    use attempt <- retry.with_retries(backoff: 1000, attempts: 3)
    case attempt {
      0 -> Nil
      _ -> io.println("  Retrying...")
    }

    hackney.send(request)
    |> result.map_error(error.HttpError)
    |> result.try(error.ensure_status(_, is: 200))
  })

  use info <- result.try(
    parse_track_page(response.body)
    |> result.replace_error(error.UnexpectedHtml),
  )

  Ok(info)
}

pub fn parse_track_page(html: String) -> Result(Information, Nil) {
  let stat_name =
    soup.element([soup.with_class("stat-name")])
    |> soup.return(soup.text_content())
    |> soup.map(string.concat)
    |> soup.map(string.trim)

  let stat_number =
    soup.element([soup.with_class("current-number")])
    |> soup.return(soup.text_content())
    |> soup.map(string.concat)
    |> soup.map(string.trim)
    |> soup.try_map(parse_int)

  use stats <- result.try(
    soup.elements([soup.with_class("report-stat")])
    |> soup.return(soup.merge2(stat_name, stat_number, pair.new))
    |> soup.map(dict.from_list)
    |> soup.scrape(html)
    |> result.replace_error(Nil),
  )

  use students <- result.try(dict.get(stats, "Students"))
  use submissions <- result.try(dict.get(stats, "Submissions"))
  use mentoring <- result.try(dict.get(stats, "Mentoring Discussions"))

  Ok(Information(
    students_count: students,
    submissions_count: submissions,
    mentoring_discussions_count: mentoring,
  ))
}

fn parse_int(string: String) -> Result(Int, Nil) {
  string
  |> string.replace(",", "")
  |> int.parse
}
