import gleam/erlang/process

/// Retry a function until it succeeds or the maximum number of retries is
/// reached.
/// 
/// The function is called with the number of retries so far, starting at 0.
/// 
/// Between retries the process is suspended for a period of time determined by
/// the backoff unit multiplied by the number of retries.
///
pub fn with_retries(
  backoff backoff: Int,
  attempts attempts: Int,
  run attempt: fn(Int) -> Result(t, e),
) -> Result(t, e) {
  try(backoff, attempts, 0, attempt)
}

fn try(
  backoff: Int,
  attempts: Int,
  retries: Int,
  attempt: fn(Int) -> Result(t, e),
) -> Result(t, e) {
  case attempt(retries) {
    Ok(value) -> Ok(value)

    Error(_) if retries < attempts -> {
      process.sleep(backoff * retries)
      try(backoff, attempts, retries + 1, attempt)
    }

    Error(error) -> Error(error)
  }
}
