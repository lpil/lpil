import app/pages
import app/web.{type Context}
import gleam/bool
import lustre/element
import wisp.{type Request, type Response}

/// The web router, dispatching requests to different handler functions
/// depending on their path.
///
pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- middleware(req, ctx)

  case wisp.path_segments(req) {
    [] -> pages.home()
    _ -> wisp.not_found()
  }
}

/// The middleware stack that is applied to all requests before they are
/// routed to a suitable handler.
///
pub fn middleware(
  req: wisp.Request,
  ctx: Context,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  let req = wisp.method_override(req)
  use <- wisp.serve_static(req, under: "/static", from: ctx.static)
  use <- wisp.serve_static(req, under: "/static", from: ctx.lustre_ui_static)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)

  handle_request(req)
  |> default_responses
}

/// Return a default response when the response from the router has an empty
/// body, likely meaning the request was invalid and was rejected by some
/// middleware.
///
pub fn default_responses(resp: wisp.Response) -> wisp.Response {
  use <- bool.guard(when: resp.body != wisp.Empty, return: resp)
  let body = fn(html) {
    html
    |> element.to_document_string_builder
    |> wisp.html_body(resp, _)
  }

  case resp.status {
    404 -> body(pages.not_found_html())
    405 -> body(pages.not_found_html())
    400 -> body(pages.bad_request_html())
    422 -> body(pages.bad_request_html())
    413 -> body(pages.entity_too_large_html())
    500 -> body(pages.internal_server_error_html())
    _ -> wisp.redirect("/")
  }
}
