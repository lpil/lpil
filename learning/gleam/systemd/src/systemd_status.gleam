import gleam/string
import gleam/option.{type Option, None, Some}
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
    load_state: LoadState,
    active_state: ActiveState,
    sub_state: String,
    result: String,
    description: Option(String),
    state_change_timestamp: Option(String),
    active_enter_timestamp: Option(String),
    active_exit_timestamp: Option(String),
    inactive_enter_timestamp: Option(String),
    inactive_exit_timestamp: Option(String),
  )
}

// TODO: document
pub type ActiveState {
  Active
  Reloading
  Inactive
  Failed
  Activating
  Deactivating
}

// TODO: document
pub type LoadState {
  Loaded
  NotFound
  BadSetting
  LoadError
  Masked
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
  use type_ <- result.try(property(props, "Type", service_type))
  use load_state <- result.try(property(props, "LoadState", load_state))
  use active_state <- result.try(property(props, "ActiveState", active_state))
  use sub_state <- result.try(property(props, "SubState", Ok))
  use result <- result.try(property(props, "Result", Ok))

  Ok(Service(
    id: id,
    type_: type_,
    load_state: load_state,
    active_state: active_state,
    sub_state: sub_state,
    result: result,
    description: option.from_result(dict.get(props, "Description")),
    state_change_timestamp: optional_time(props, "StateChangeTimestamp"),
    active_enter_timestamp: optional_time(props, "ActiveEnterTimestamp"),
    active_exit_timestamp: optional_time(props, "ActiveExitTimestamp"),
    inactive_enter_timestamp: optional_time(props, "InactiveEnterTimestamp"),
    inactive_exit_timestamp: optional_time(props, "InactiveExitTimestamp"),
  ))
}

fn service_type(input: String) -> Result(ServiceType, String) {
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

fn load_state(input: String) -> Result(LoadState, String) {
  case input {
    "loaded" -> Ok(Loaded)
    "not-found" -> Ok(NotFound)
    "bad-setting" -> Ok(BadSetting)
    "load-error" -> Ok(LoadError)
    "masked" -> Ok(Masked)
    _ -> Error("Unknown load state: " <> input)
  }
}

fn active_state(input: String) -> Result(ActiveState, String) {
  case input {
    "active" -> Ok(Active)
    "reloading" -> Ok(Reloading)
    "inactive" -> Ok(Inactive)
    "failed" -> Ok(Failed)
    "activating" -> Ok(Activating)
    "deactivating" -> Ok(Deactivating)
    _ -> Error("Unknown active state: " <> input)
  }
}

fn optional_time(
  properties: Dict(String, String),
  name: String,
) -> Option(String) {
  case dict.get(properties, name) {
    Ok("n/a") | Error(_) -> None
    Ok(time) -> Some(time)
  }
}
