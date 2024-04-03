import app/router
import app/web.{Context}
import filepath
import gleam/erlang/process
import gleam/pgo
import mist
import wisp

pub fn main() {
  wisp.configure_logger()

  // Load static values that are shared between all requests

  // TODO: load from env
  let secret_key_base = wisp.random_string(64)

  let assert Ok(static) = wisp.priv_directory("app")
  let assert Ok(lustre_ui_static) = wisp.priv_directory("lustre_ui")

  let ctx =
    Context(
      db: start_database_pool(),
      static: filepath.join(static, "/static"),
      lustre_ui_static: filepath.join(lustre_ui_static, "/static"),
    )

  let assert Ok(_) =
    wisp.mist_handler(router.handle_request(_, ctx), secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

fn start_database_pool() -> pgo.Connection {
  let config =
    pgo.Config(
      ..pgo.default_config(),
      host: "localhost",
      database: "wisp_starter_kit",
      pool_size: 15,
    )
  pgo.connect(config)
}
