import gleam/option.{None, Option, Some}

pub type Bottle {
  Bottle(full: Bool)
}

pub type Station {
  Station(is_filling: Bool, bottle: Option(Bottle))
}

/// Starts the flow of bubble bath at the required station.
/// Effective instantaneously.
///
/// Overflows if there if the given station has no bottle.
///
/// After `time_to_fill(station)` + maybe_random_int() seconds it fills the
/// bottle.
///
/// After some margin period after reaching the full point it overflows.
/// TODO: what is the allowable margin period?
///
pub fn start_filling(station: Station) -> Station {
  // TODO: At a point in future set the bottle to full
  // TODO: At a point in future overflow the bottle
  Station(..station, is_filling: True)
}

/// Stops the flow of bubble bath at the required station.
/// Effective instantaneously.
///
pub fn stop_filling(station: Station) -> Station {
  // TODO: Cancel the future event of filling the bottle
  // TODO: Cancel the future event of overflowing the bottle
  Station(..station, is_filling: False)
}

/// Provides an estimate in seconds of the time left to fillthe bottle at
/// therequired station, or 0 if the bottle is full; the actual time will be
/// greater or equal to thisestimate.
///
/// Effective instantaneously.
///
pub fn time_to_fill(_station: Station) -> Int {
  // TODO
  0
}

/// Returns true if there is a bottle at the requiredstation; false otherwise.
/// Effective instantaneously.
///
pub fn is_bottle(station: Station) -> Bool {
  option.is_some(station.bottle)
}

/// Returns true if there is a bottle being filled at the required station;
/// false otherwise. Effectiveinstantaneously
///
pub fn is_filling(station: Station) -> Bool {
  station.is_filling
}

pub external fn sleep(Int) -> Nil =
  "timer" "sleep"

/// Causes the robot to fetch a new bottle and place it atthe required station.
///
/// This command takes 1 second tocomplete.
///
pub fn place_bottle(station: Station) -> Station {
  sleep(1000)
  let bottle = Bottle(full: False)
  Station(..station, bottle: Some(bottle))
}

/// Causes the robot to remove a bottle from the requiredstation and deliver it
/// for packing.
///
/// This command takes 2 seconds to complete.
///
pub fn remove_bottle(station: Station) -> Station {
  sleep(2000)
  Station(..station, bottle: None)
}

pub fn new_station() -> Station {
  Station(is_filling: False, bottle: None)
}
