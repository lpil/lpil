import app/web
import lustre/attribute.{attribute, href}
import lustre/element.{text}
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
