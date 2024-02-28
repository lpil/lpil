import gleam/string
import gleam/result
import gleam/dict.{type Dict}
import gleam/list

// systemctl show *  --no-pager -p Id

// - [ ] Service Units
//   - [x] Simple
//   - [ ] Forking
//   - [x] Oneshot
//   - [ ] DBus
//   - [ ] Notify
//   - [ ] Idle
//   - [ ] Exec
//   - [ ] RemainAfterExit
// - [ ] Socket Units
// - [ ] Device Units
// - [ ] Mount Units
// - [ ] Automount Units
// - [ ] Target Units
// - [ ] Snapshot Units
// - [ ] Timer Units
// - [ ] Path Units
// - [ ] Slice Units

// TODO: document
pub type Service {
  Service(
    id: String,
    type_: ServiceType,
    // TODO: refine this to be a custom type
    active_state: String,
    // TODO: refine this to be a custom type
    sub_state: String,
    result: String,
  )
}

// TODO: document
pub type ServiceType {
  SimpleService
  OneshotService
  OtherServiceType(name: String)
}

// TODO: document
pub fn parse_service(input: String) -> Result(Service, String) {
  let props =
    input
    |> string.split("\n")
    |> list.map(string.trim)
    |> list.filter_map(string.split_once(_, "="))
    |> dict.from_list

  use id <- result.try(property(props, "Id", Ok))
  use type_ <- result.try(property(props, "Type", parse_service_type))
  use active_state <- result.try(property(props, "ActiveState", Ok))
  use sub_state <- result.try(property(props, "SubState", Ok))
  use result <- result.try(property(props, "Result", Ok))

  Ok(Service(
    id: id,
    type_: type_,
    active_state: active_state,
    sub_state: sub_state,
    result: result,
  ))
}

fn parse_service_type(input: String) -> Result(ServiceType, String) {
  case input {
    "simple" -> Ok(SimpleService)
    "oneshot" -> Ok(OneshotService)
    _ -> Ok(OtherServiceType(input))
  }
}

fn property(
  properties: Dict(String, String),
  name: String,
  decode: fn(String) -> Result(t, String),
) -> Result(t, String) {
  case dict.get(properties, name) {
    Ok(value) -> decode(value)
    _ -> Error("Property " <> name <> " not found")
  }
}
