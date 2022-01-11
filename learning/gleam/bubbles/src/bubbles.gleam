import factory.{Station}
import gleam/list
import gleam/bool
import gleam/int

const remove_bottle_time_cost = 2

const place_bottle_time_cost = 1

pub type Robot {
  Robot(
    to_fill_stations: List(Station),
    full_stations: List(Station),
    filling_stations: List(Station),
    bottleless_stations: List(Station),
  )
}

pub fn main() {
  let robot =
    Robot(
      full_stations: [],
      to_fill_stations: [],
      filling_stations: [],
      bottleless_stations: list.repeat(factory.new_station(), times: 8),
    )
  work_loop(robot)
}

fn work_loop(robot: Robot) {
  // Stop filling any stations that are now full
  let tuple(full, filling) = stop_full_station(robot.filling_stations)
  let full = list.append(robot.full_stations, full)

  // Start filling any waiting bottles
  let filling =
    robot.to_fill_stations
    |> list.map(factory.start_filling)
    |> list.append(filling)

  Robot(..robot, filling_stations: filling, full_stations: full)
  |> perform_expensive_work
  |> work_loop
}

fn perform_expensive_work(robot: Robot) -> Robot {
  let deadline = next_deadline(robot.filling_stations)

  case robot {
    // No time left, return to performing instant tasks
    _ if deadline <= 0 -> robot

    // We have a station with a full bottle and time to replace it
    Robot(full_stations: [full, ..rest], ..) if deadline >= remove_bottle_time_cost -> {
      let station = factory.remove_bottle(full)
      let bottleless = [station, ..robot.bottleless_stations]
      let robot =
        Robot(..robot, bottleless_stations: bottleless, full_stations: rest)
      perform_expensive_work(robot)
    }

    // We have a station without a bottle and time to replace it
    Robot(bottleless_stations: [station, ..rest], ..) if deadline >= place_bottle_time_cost -> {
      let station =
        station
        |> factory.place_bottle
        |> factory.start_filling
      let robot =
        Robot(
          ..robot,
          bottleless_stations: rest,
          filling_stations: [station, ..robot.filling_stations],
        )
      perform_expensive_work(robot)
    }

    // No more work to do, wait 1 second and return to performing instant tasks
    _ -> {
      factory.sleep(1000)
      robot
    }
  }
}

fn next_deadline(filling: List(Station)) -> Int {
  let soonest = fn(station, deadline) {
    station
    |> factory.time_to_fill
    |> int.min(deadline)
  }
  list.fold(filling, 100, soonest)
}

fn stop_full_station(
  filling: List(Station),
) -> tuple(List(Station), List(Station)) {
  let is_full = fn(s) { factory.time_to_fill(s) == 0 }
  let is_not_full = fn(s) { bool.negate(is_full(s)) }

  let filling = list.filter(filling, is_not_full)
  let full =
    filling
    |> list.filter(is_full)
    |> list.map(factory.stop_filling)

  tuple(full, filling)
}
