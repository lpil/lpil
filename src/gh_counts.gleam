import gleam/list
import gleam/set.{Set}
import gleam/string
import gleam/int
import gleam/io
import gleam/http
import gleam/httpc
import gleam/result
import gleam/dynamic.{Dynamic}
import gleam/atom.{Atom}

pub external type CharList

pub external fn sleep(Int) -> Dynamic =
  "timer" "sleep"

pub fn main(args: List(CharList)) {
  start_application_and_deps(atom.create_from_string("gh_counts"))
  let args = list.map(args, char_list_to_string)
  assert Ok(token) = parse_args(args)
  assert Ok(counts) = count_github_gleam_uses(token)
  print_counts(counts)
}

fn count_github_gleam_uses(token: String) -> Result(Counts, String) {
  loop(SearchState(
    counts: Counts(set.new(), set.new(), set.new()),
    token: token,
    page: 0,
  ))
}

fn loop(state: SearchState) -> Result(Counts, String) {
  io.print(".")
  try json = query_search_api(state)
  try page = parse_search_json(json)
  case page {
    [] -> {
      io.println("")
      Ok(state.counts)
    }
    _ -> {
      let counts = count_page(page, state.counts)
      loop(SearchState(..state, page: state.page + 1, counts: counts))
    }
  }
}

fn count_page(page: List(SearchEntry), counts: Counts) {
  case page {
    [] -> counts
    [entry, ..page] -> {
      let file = string.concat([entry.owner, "/", entry.file])
      let counts =
        Counts(
          files: set.insert(counts.files, file),
          repos: set.insert(counts.repos, entry.repo_name),
          users: set.insert(counts.users, entry.owner),
        )
      count_page(page, counts)
    }
  }
}

fn parse_search_json(json: String) -> Result(List(SearchEntry), String) {
  let item = fn(json) {
    try path = dynamic.field(json, "path")
    try path = dynamic.string(path)
    try repo = dynamic.field(json, "repository")
    try owner = dynamic.field(repo, "owner")
    try owner = dynamic.field(owner, "login")
    try owner = dynamic.string(owner)
    try repo_name = dynamic.field(repo, "full_name")
    try repo_name = dynamic.string(repo_name)
    let file = string.concat([repo_name, "/", path])
    Ok(SearchEntry(repo_name: repo_name, owner: owner, file: file))
  }
  let json = json_decode(json)
  try items = dynamic.field(json, "items")
  dynamic.typed_list(of: item, from: items)
}

type SearchEntry {
  SearchEntry(repo_name: String, owner: String, file: String)
}

fn query_search_api(state: SearchState) -> Result(String, String) {
  let auth_header = string.append("bearer ", state.token)
  let query = [
    tuple("q", "extension:gleam"),
    tuple("page", int.to_string(state.page)),
  ]
  try resp =
    http.default_req()
    |> http.set_method(http.Get)
    |> http.set_host("api.github.com")
    |> http.set_path("/search/code")
    |> http.set_query(query)
    |> http.prepend_req_header("user-agent", "gleam contributors")
    |> http.prepend_req_header("authorization", auth_header)
    |> http.prepend_req_header("content-type", "application/json")
    |> httpc.send
    |> result.replace_error("HTTP Request failed")

  // Only 30 requests per minute are permitted to the search API
  // https://docs.github.com/en/rest/reference/search#rate-limit
  sleep(1000 * 60 / 30)

  case resp.status {
    200 -> Ok(resp.body)
    _ -> {
      io.debug(resp)
      Error("Unexpected HTTP status")
    }
  }
}

type SearchState {
  SearchState(counts: Counts, token: String, page: Int)
}

type Counts {
  Counts(users: Set(String), repos: Set(String), files: Set(String))
}

fn print_counts(counts: Counts) {
  let print = fn(name, set) {
    name
    |> string.append(": ")
    |> string.append(int.to_string(set.size(set)))
    |> io.println
  }
  print("users", counts.users)
  print("repos", counts.repos)
  print("files", counts.files)
}

fn parse_args(args: List(String)) -> Result(String, String) {
  case args {
    [token] -> Ok(token)
    _ -> Error("usage: gh_counts $GITHUB_TOKEN")
  }
}

external fn char_list_to_string(CharList) -> String =
  "erlang" "list_to_binary"

pub external fn json_decode(String) -> Dynamic =
  "jsone" "decode"

external fn start_application_and_deps(Atom) -> Dynamic =
  "application" "ensure_all_started"
