import filepath
import log/exercise.{type ExerciseLog}
import wisp

pub type StaticContext {
  StaticContext(priv: String, exercise_log: ExerciseLog)
}

pub type RequestContext {
  RequestContext(exercise_log: ExerciseLog)
}

pub fn handle_request(
  req: wisp.Request,
  ctx: StaticContext,
  handle_request: fn(wisp.Request, RequestContext) -> wisp.Response,
) -> wisp.Response {
  // Serve static assets
  let static_directory = filepath.join(ctx.priv, "static")
  use <- wisp.serve_static(req, "/static", static_directory)

  // Permit browsers to simulate methods other than GET and POST using the
  // `_method` query parameter.
  let req = wisp.method_override(req)

  // Log information about the request and response.
  use <- wisp.log_request(req)

  // Return a default 500 response if the request handler crashes.
  use <- wisp.rescue_crashes

  // Rewrite HEAD requests to GET requests.
  use req <- wisp.handle_head(req)

  // Known-header based CSRF protection for non-HEAD/GET requests
  use req <- wisp.csrf_known_header_protection(req)

  // Handle the request!
  let ctx = RequestContext(exercise_log: ctx.exercise_log)
  handle_request(req, ctx)
}
