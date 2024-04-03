import app/router
import app/web.{Context}
import filepath
import gleam/erlang/process
import mist
import wisp

pub fn main() {
  wisp.configure_logger()

  // TODO: load from env
  let secret_key_base = wisp.random_string(64)

  let assert Ok(static) = wisp.priv_directory("app")
  let assert Ok(lustre_ui_static) = wisp.priv_directory("lustre_ui")

  let ctx =
    Context(
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
