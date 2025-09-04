import envoy
import gleam/erlang/process
import log/exercise
import log/web
import log/web/router
import mist
import storail
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  let assert Ok(secret_key_base) = envoy.get("SECRET_KEY_BASE")
    as "SECRET_KEY_BASE environment variable must be set"
  let assert Ok(storage_path) = envoy.get("STORAGE_PATH")
    as "STORAGE_PATH environment variable must be set"

  // Locate priv directories
  let assert Ok(priv) = wisp.priv_directory("log") as "Getting priv directory"

  // Prepare the storage
  let storage_config = storail.Config(storage_path:)
  let exercise_log = exercise.open_log(storage_config)

  let ctx = web.StaticContext(priv:, exercise_log:)

  // Start the Mist web server.
  let assert Ok(_) =
    mist.new(fn(request) {
      let handler = web.handle_request(_, ctx, router.handle_request)
      let handler = wisp_mist.handler(handler, secret_key_base)
      handler(request)
    })
    |> mist.port(8000)
    |> mist.start
    as "Starting Mist server"

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}
