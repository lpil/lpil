import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import gleam/string
import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import log/error.{type Error}
import log/storage
import storail

pub type Session {
  Session(
    /// Where the session took place. e.g. Regent's Park
    location: String,
    /// Who the session was lead by, if anyone.
    coach: Option(String),
    /// When the session started.
    start: Timestamp,
    /// How long it went on for. If `None` then it's currently in progress.
    duration: Option(Duration),
    /// Any addition info such as depth added.
    detail: Option(String),
    /// The sets the session contained
    activities: List(Activity),
  )
}

pub type Activity {
  /// Training a skill for duration of time
  SkillTraining(
    /// e.g. Soft-acro, bouldering
    discipline: String,
    /// e.g. Front layout, tornado kick
    skills: List(String),
    /// How long spent training.
    duration: Duration,
  )
  /// Performing a conditioning exercise
  Conditioning(
    // e.g. pike push ups, pancake.
    name: String,
    sets: List(Set),
  )
}

pub type Set {
  /// Sets held for time. e.g. L-sit.
  IsometricSet(
    duration: Duration,
    /// Added weight, in kilograms.
    weight: Option(Float),
  )
  /// Sets performed for repetitions. e.g. pull-ups.
  IsotonicSet(
    /// Number of repetitions.
    reps: Int,
    /// Additional weight in kilograms.
    weight: Option(Float),
  )
}

pub opaque type ExerciseLog {
  ExerciseLog(sessions: storail.Collection(Session))
}

/// Open the exercise log, so data can be read from and written to it.
///
pub fn open_log(config: storail.Config) -> ExerciseLog {
  let sessions =
    storail.Collection(
      name: "sessions",
      to_json: session_to_json,
      decoder: session_decoder(),
      config:,
    )
  ExerciseLog(sessions:)
}

/// List all recorded sessions, from newest to oldest.
///
pub fn list_sessions(log: ExerciseLog) -> Result(List(Timestamp), Error) {
  use keys <- result.try(storage.list(log.sessions, []))
  let assert Ok(keys) = list.try_map(keys, int.parse) as "session keys are ints"
  keys
  |> list.sort(order.reverse(int.compare))
  |> list.map(timestamp.from_unix_seconds)
  |> Ok
}

/// Save a session in the log. If there already is a session with that
/// timestamp (second precision) then the previous one will be replaced.
///
pub fn write_session(log: ExerciseLog, session: Session) -> Result(Nil, Error) {
  log.sessions
  |> storail.key(session_id(session))
  |> storage.write(session)
}

/// Read a session from the log.
///
pub fn read_session(
  log: ExerciseLog,
  session_id: String,
) -> Result(Session, Error) {
  log.sessions
  |> storail.key(session_id)
  |> storage.read
}

/// Append a set to an activity in the workflow. Returns an error if there's no
/// conditioning activity with that name.
///
/// Does not write to storage, only updates the given data structure.
///
pub fn add_set(
  session: Session,
  activity_name: String,
  set: Set,
) -> Result(Session, Nil) {
  let #(updated, activities) =
    list.fold(session.activities, #(False, []), fn(acc, activity) {
      case activity {
        Conditioning(name:, sets:) if name == activity_name -> {
          let activity = Conditioning(name:, sets: list.append(sets, [set]))
          #(True, [activity, ..acc.1])
        }
        Conditioning(..) | SkillTraining(..) -> #(acc.0, [activity, ..acc.1])
      }
    })
  case updated {
    False -> Error(Nil)
    True -> Ok(Session(..session, activities: list.reverse(activities)))
  }
}

/// Traverse sessions from most recent to least recent.
///
pub fn fold_sessions(
  log: ExerciseLog,
  acc: t,
  reduce: fn(t, Session) -> list.ContinueOrStop(t),
) -> Result(t, Error) {
  use keys <- result.try(storage.list(log.sessions, []))
  // Most recent to least recent
  let keys = list.sort(keys, order.reverse(string.compare))
  fold_sessions_loop(keys, log, acc, reduce)
}

fn fold_sessions_loop(
  keys: List(String),
  log: ExerciseLog,
  acc: t,
  reduce: fn(t, Session) -> list.ContinueOrStop(t),
) -> Result(t, Error) {
  case keys {
    [] -> Ok(acc)
    [key, ..keys] -> {
      use session <- result.try(read_session(log, key))
      case reduce(acc, session) {
        list.Continue(acc) -> fold_sessions_loop(keys, log, acc, reduce)
        list.Stop(acc) -> Ok(acc)
      }
    }
  }
}

/// Get the N most recent sessions
///
pub fn most_recent_sessions(
  log: ExerciseLog,
  count: Int,
) -> Result(List(Session), Error) {
  let sessions =
    fold_sessions(log, #(count, []), fn(acc, session) {
      let #(count, sessions) = acc
      case count {
        0 -> list.Stop(#(0, sessions))
        1 -> list.Stop(#(0, [session, ..sessions]))
        _ -> list.Continue(#(count - 1, [session, ..sessions]))
      }
    })
  case sessions {
    Ok(#(_, sessions)) -> Ok(list.reverse(sessions))
    Error(error) -> Error(error)
  }
}

//
// Encoding and decoding
//

fn set_to_json(set: Set) -> json.Json {
  case set {
    IsometricSet(duration:, weight:) ->
      json.object([
        #("type", json.string("isometric_set")),
        #("duration", duration_to_json(duration)),
        #("weight", case weight {
          option.None -> json.null()
          option.Some(value) -> json.float(value)
        }),
      ])
    IsotonicSet(reps:, weight:) ->
      json.object([
        #("type", json.string("isotonic")),
        #("reps", json.int(reps)),
        #("weight", case weight {
          option.None -> json.null()
          option.Some(value) -> json.float(value)
        }),
      ])
  }
}

fn set_decoder() -> decode.Decoder(Set) {
  let isometric = {
    use duration <- decode.field("duration", duration_decoder())
    use weight <- decode.field("weight", decode.optional(decode.float))
    decode.success(IsometricSet(duration:, weight:))
  }
  let isotonic = {
    use reps <- decode.field("reps", decode.int)
    use weight <- decode.field("weight", decode.optional(decode.float))
    decode.success(IsotonicSet(reps:, weight:))
  }
  decode.one_of(isometric, [isotonic])
}

pub fn session_id(session: Session) -> String {
  session.start
  |> timestamp.to_unix_seconds
  |> float.round
  |> int.to_string
}

fn session_to_json(session: Session) -> json.Json {
  let Session(location:, start:, duration:, coach:, detail:, activities:) =
    session
  []
  |> json_property("start", start, timestamp_to_json)
  |> json_property("location", location, json.string)
  |> json_optional_property("coach", coach, json.string)
  |> json_optional_property("duration", duration, duration_to_json)
  |> json_optional_property("detail", detail, json.string)
  |> json_property("activities", activities, json.array(_, activity_to_json))
  |> list.reverse
  |> json.object
}

fn json_property(
  properties: List(#(String, json.Json)),
  name: String,
  value: t,
  to_json: fn(t) -> json.Json,
) -> List(#(String, json.Json)) {
  [#(name, to_json(value)), ..properties]
}

fn json_optional_property(
  properties: List(#(String, json.Json)),
  name: String,
  value: Option(t),
  to_json: fn(t) -> json.Json,
) -> List(#(String, json.Json)) {
  case value {
    option.None -> properties
    option.Some(value) -> [#(name, to_json(value)), ..properties]
  }
}

fn session_decoder() -> decode.Decoder(Session) {
  use location <- decode.field("location", decode.string)
  use start <- decode.field("start", timestamp_decoder())
  use duration <- decode.optional_field(
    "duration",
    option.None,
    decode.optional(duration_decoder()),
  )
  use coach <- decode.optional_field(
    "coach",
    option.None,
    decode.optional(decode.string),
  )
  use detail <- decode.optional_field(
    "detail",
    option.None,
    decode.optional(decode.string),
  )
  use activities <- decode.field("activities", decode.list(activity_decoder()))
  decode.success(Session(
    location:,
    start:,
    duration:,
    coach:,
    detail:,
    activities:,
  ))
}

fn timestamp_to_json(timestamp: Timestamp) -> json.Json {
  timestamp
  |> timestamp.to_unix_seconds
  |> float.round
  |> json.int
}

fn timestamp_decoder() -> decode.Decoder(Timestamp) {
  decode.int
  |> decode.map(timestamp.from_unix_seconds)
}

fn duration_to_json(duration: Duration) -> json.Json {
  duration
  |> duration.to_seconds
  |> float.round
  |> json.int
}

fn duration_decoder() -> decode.Decoder(Duration) {
  decode.int
  |> decode.map(duration.seconds)
}

fn activity_decoder() -> decode.Decoder(Activity) {
  let skill_training = {
    use discipline <- decode.field("discipline", decode.string)
    use skills <- decode.field("skills", decode.list(decode.string))
    use duration <- decode.field("duration", duration_decoder())
    decode.success(SkillTraining(discipline:, skills:, duration:))
  }
  let conditioning = {
    use name <- decode.field("name", decode.string)
    use sets <- decode.field("sets", decode.list(set_decoder()))
    decode.success(Conditioning(name:, sets:))
  }
  decode.one_of(skill_training, [conditioning])
}

fn activity_to_json(activity: Activity) -> json.Json {
  case activity {
    SkillTraining(discipline:, skills:, duration:) ->
      json.object([
        #("type", json.string("skill_training")),
        #("discipline", json.string(discipline)),
        #("skills", json.array(skills, json.string)),
        #("duration", duration_to_json(duration)),
      ])
    Conditioning(name:, sets:) ->
      json.object([
        #("type", json.string("conditioning")),
        #("name", json.string(name)),
        #("sets", json.array(sets, set_to_json)),
      ])
  }
}
