import app/web
import lustre/attribute.{attribute, href}
import lustre/element.{type Element, text}
import lustre/element/html
import lustre/ui
import lustre/ui/icon
import lustre/ui/prose
import wisp.{type Response}

pub fn home() -> Response {
  let styles = [#("width", "80ch"), #("margin", "0 auto"), #("padding", "2rem")]
  [
    ui.stack([attribute.style(styles)], [
      ui.prose([prose.full()], [
        html.h1([], [text("Hello, world!")]),
        html.a([href("https://github.com/lpil/wisp-starter-kit")], [
          icon.github_logo([]),
          text("Source code"),
        ]),
        html.a([href("https://discord.gg/Fm8Pwmy")], [
          icon.discord_logo([]),
          text("Join the Gleam Discord"),
        ]),
      ]),
    ]),
  ]
  |> web.layout
  |> web.html_response(200)
}

fn error_page_html(message: String, content: List(Element(t))) -> Element(t) {
  [html.h1([], [text(message)]), ..content]
  |> web.layout
}

pub fn not_found_html() -> Element(t) {
  "Sorry, no page was fonud at this location."
  |> error_page_html([])
}

pub fn bad_request_html() -> Element(t) {
  "Sorry, there was a problem with your request. Please try again later."
  |> error_page_html([])
}

pub fn entity_too_large_html() -> Element(t) {
  "Sorry, your browser sent more data than we were able to handle. Please try
  again later."
  |> error_page_html([])
}

pub fn internal_server_error_html() -> Element(t) {
  "Sorry, there was a problem handling your request. No data has been lost.
  Please try again later."
  |> error_page_html([])
}
