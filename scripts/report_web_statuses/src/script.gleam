import glatus
import gleam/httpc
import gleam/dynamic.{type Dynamic}
import gleam/result
import gleam/string
import gleam/list
import gleam/int
import gleam/bool
import gleam/io

const host = "status.lpil.uk"

pub fn main() {
  let #(#(year, month, day), _) = now()
  let today =
    [
      string.pad_left(int.to_string(year), 4, "0"),
      string.pad_left(int.to_string(month), 2, "0"),
      string.pad_left(int.to_string(day), 2, "0"),
    ]
    |> string.join("-")

  let request = glatus.statuses_request(host, page: 1)
  let assert Ok(response) = httpc.send(request)
  let assert Ok(endpoints) = glatus.handle_statuses_response(response)
  let count = int.to_string(list.length(endpoints))

  io.println("Found " <> count <> " endpoints")

  let problem_endpoints =
    endpoints
    |> list.filter_map(fn(endpoint) {
      let request = glatus.endpoint_request(host, endpoint.key, page: 1)
      let assert Ok(response) = httpc.send(request)
      let assert Ok(endpoint) = glatus.handle_endpoint_response(response)

      let unhealthy =
        endpoint.events
        |> list.filter(fn(event) { event.type_ == glatus.Unhealthly })
        |> list.filter(fn(event) { string.starts_with(event.timestamp, today) })

      let hostname =
        endpoint.results
        |> list.find_map(fn(result) { Ok(result.hostname) })
        |> result.unwrap(endpoint.name)

      case unhealthy {
        [] | [_] -> Error(Nil)
        _ -> Ok(hostname)
      }
    })

  let count = int.to_string(list.length(problem_endpoints))
  io.println("Found " <> count <> " problem endpoints")

  use <- bool.guard(when: problem_endpoints == [], return: Nil)

  let list =
    problem_endpoints
    |> list.map(string.append("- https://", _))
    |> string.join("\n")

  io.println("")
  io.println("Report sites:")
  io.println("- https://downdetector.co.uk/status/gnetwork/")
  io.println("- https://www.g.network/account/cases/create")
  io.println("- https://mail.zoho.com/zm/#compose (support@g.network)")

  io.println("")
  io.println("Details for my G Network account:")
  io.println("Name: Louis Pilfold")
  io.println("Account email: gnetwork@lpil.uk")

  io.println("")
  io.println("Websites that are IP blocked by G.Network:")
  io.println(list)
}

@external(erlang, "calendar", "universal_time")
pub fn now() -> #(#(Int, Int, Int), Dynamic)
