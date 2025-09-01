import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import log/error.{type Error}
import log/storage
import storail

pub type Set {
  Set(
    /// Exercise name.
    name: String,
    /// Number of repetitions.
    reps: Int,
    /// Additional extra weight in kilograms. Will be 0 for bodyweight.
    weight: Float,
    /// Any addition info such as depth added.
    detail: String,
  )
}

pub type Workout {
  Workout(
    /// Where the workout was performed.
    location: String,
    /// When the workout started.
    start: Timestamp,
    /// How long it went on for. If `None` then it's currently in progress.
    duration: Option(Duration),
  )
}

pub opaque type ExerciseLog {
  ExerciseLog(
    workouts: storail.Collection(Workout),
    sets: storail.Collection(Set),
  )
}

/// Open the exercise log, so data can be read from and written to it.
///
pub fn open_log(config: storail.Config) -> ExerciseLog {
  let sets =
    storail.Collection(
      name: "exercise-sets",
      to_json: set_to_json,
      decoder: set_decoder(),
      config:,
    )
  let workouts =
    storail.Collection(
      name: "workouts",
      to_json: workout_to_json,
      decoder: workout_decoder(),
      config:,
    )
  ExerciseLog(sets:, workouts:)
}

/// List all recorded workouts, from newest to oldest.
///
pub fn list_workouts(log: ExerciseLog) -> Result(List(Timestamp), Error) {
  use keys <- result.try(storage.list(log.workouts, []))
  let assert Ok(keys) = list.try_map(keys, int.parse) as "workout keys are ints"
  keys
  |> list.sort(order.reverse(int.compare))
  |> list.map(timestamp.from_unix_seconds)
  |> Ok
}

/// Save a workout in the log. If there already is a workout with that
/// timestamp (second precision) then the previous one will be replaced.
///
pub fn write_workout(log: ExerciseLog, workout: Workout) -> Result(Nil, Error) {
  log.workouts
  |> storail.key(int.to_string(workout_id(workout)))
  |> storage.write(workout)
}

/// Save a set in the log.
///
pub fn write_set(
  log: ExerciseLog,
  workout: Workout,
  set: Set,
) -> Result(Nil, Error) {
  log.workouts
  |> storail.key(int.to_string(workout_id(workout)))
  |> storage.write(workout)
}

fn workout_id(workout: Workout) -> Int {
  workout.start
  |> timestamp.to_unix_seconds
  |> float.round
}

fn set_to_json(set: Set) -> json.Json {
  let Set(name:, reps:, weight:, detail:) = set
  let properties = case detail {
    "" -> []
    _ -> [#("detail", json.string(detail))]
  }
  json.object([
    #("name", json.string(name)),
    #("reps", json.int(reps)),
    #("weight", json.float(weight)),
    ..properties
  ])
}

fn set_decoder() -> decode.Decoder(Set) {
  use name <- decode.field("name", decode.string)
  use reps <- decode.field("reps", decode.int)
  use weight <- decode.field("weight", decode.float)
  use detail <- decode.field("detail", decode.string)
  decode.success(Set(name:, reps:, weight:, detail:))
}

fn workout_to_json(workout: Workout) -> json.Json {
  let Workout(location:, start:, duration:) = workout
  let properties = case duration {
    option.None -> []
    option.Some(d) -> [#("duration", duration_to_json(d))]
  }
  json.object([
    #("location", json.string(location)),
    #("start", timestamp_to_json(start)),
    ..properties
  ])
}

fn workout_decoder() -> decode.Decoder(Workout) {
  use location <- decode.field("location", decode.string)
  use start <- decode.field("start", decode_timestamp())
  use duration <- decode.optional_field(
    "duration",
    option.None,
    decode_duration() |> decode.map(option.Some),
  )
  decode.success(Workout(location:, start:, duration:))
}

fn timestamp_to_json(timestamp: Timestamp) -> json.Json {
  timestamp
  |> timestamp.to_unix_seconds
  |> float.round
  |> json.int
}

fn decode_timestamp() -> decode.Decoder(Timestamp) {
  decode.int
  |> decode.map(timestamp.from_unix_seconds)
}

fn duration_to_json(duration: Duration) -> json.Json {
  duration
  |> duration.to_seconds
  |> float.round
  |> json.int
}

fn decode_duration() -> decode.Decoder(Duration) {
  decode.int
  |> decode.map(duration.seconds)
}
