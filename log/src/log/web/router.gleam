import gleam/http.{Get, Post}
import gleam/list
import gleam/option
import gleam/time/calendar
import gleam/time/timestamp
import log/exercise
import log/web
import log/web/page
import lustre/attribute
import lustre/element/html
import wisp

pub fn handle_request(
  req: wisp.Request,
  ctx: web.RequestContext,
) -> wisp.Response {
  case req.method, wisp.path_segments(req) {
    Get, [] -> list_sessions(ctx)
    Get, ["sessions", id] -> view_session(id, ctx)
    Post, ["sessions"] -> create_session(req, ctx)
    _, _ -> wisp.not_found()
  }
}

fn list_sessions(ctx: web.RequestContext) -> wisp.Response {
  let assert Ok(sessions) = exercise.most_recent_sessions(ctx.exercise_log, 100)
    as "read sessions"
  sessions_view(sessions, ctx)
  |> wisp.html_response(200)
}

fn view_session(id: String, ctx: web.RequestContext) -> wisp.Response {
  let assert Ok(session) = exercise.read_session(ctx.exercise_log, id)
    as "read session"
  session_view(session, ctx)
  |> wisp.html_response(200)
}

fn create_session(req: wisp.Request, ctx: web.RequestContext) -> wisp.Response {
  use form <- wisp.require_form(req)
  let assert Ok(location) = list.key_find(form.values, "location")
    as "New session location"
  let coach = list.key_find(form.values, "coach") |> option.from_result
  let session =
    exercise.Session(
      location:,
      coach:,
      start: timestamp.system_time(),
      duration: option.None,
      detail: option.None,
      activities: [],
    )
  let assert Ok(_) = exercise.write_session(ctx.exercise_log, session)
    as "Write new session"
  wisp.redirect(session_href(session))
}

fn session_href(session: exercise.Session) -> String {
  "/sessions/" <> exercise.session_id(session)
}

fn sessions_view(
  sessions: List(exercise.Session),
  ctx: web.RequestContext,
) -> String {
  page.layout(ctx, [
    html.h2([], [html.text("New session")]),
    html.form([attribute.action("/sessions"), attribute.method("POST")], [
      html.fieldset([attribute.class("grid")], [
        html.label([], [
          html.text("Location"),
          html.input([
            attribute.name("location"),
            attribute.type_("text"),
            attribute.required(True),
            attribute.placeholder("Overgravity"),
          ]),
        ]),
        html.label([], [
          html.text("Coach"),
          html.input([
            attribute.name("coach"),
            attribute.type_("text"),
          ]),
        ]),
      ]),
      html.input([attribute.type_("submit"), attribute.value("Start")]),
    ]),

    html.h2([], [html.text("Past sessions")]),
    html.ol(
      [],
      list.map(sessions, fn(session) {
        let date = timestamp.to_rfc3339(session.start, calendar.utc_offset)
        let href = session_href(session)
        let text = date <> " - " <> session.location
        html.li([], [html.a([attribute.href(href)], [html.text(text)])])
      }),
    ),
  ])
}

fn session_view(session: exercise.Session, ctx: web.RequestContext) -> String {
  page.layout(ctx, [
    html.form(
      [
        attribute.action(exercise.session_id(session)),
        attribute.method("POST"),
      ],
      [
        html.fieldset([attribute.role("group")], [
          html.label([], [
            html.text("Location"),
            html.input([
              attribute.name("location"),
              attribute.type_("text"),
              attribute.required(True),
              attribute.value(session.location),
            ]),
          ]),
          html.label([], [
            html.text("Coach"),
            html.input([
              attribute.name("coach"),
              attribute.type_("text"),
              attribute.value(session.coach |> option.unwrap("")),
            ]),
          ]),
          html.label([], [
            html.text("Detail"),
            html.input([
              attribute.name("detail"),
              attribute.type_("text"),
              attribute.value(session.detail |> option.unwrap("")),
            ]),
          ]),
        ]),
        html.fieldset([attribute.role("group")], [
          html.input([attribute.type_("submit"), attribute.value("Update")]),
          html.input([attribute.type_("submit"), attribute.value("Finish")]),
        ]),
      ],
    ),
  ])
}
