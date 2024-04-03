import gleam/list
import gleam/pgo
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import wisp.{type Response}

pub type Context {
  Context(
    /// The PostgreSQL database connection pool.
    db: pgo.Connection,
    /// The directory where our applications's static assets for Lustre UI can
    /// be found, to be served to the client.
    static: String,
    /// The directory where the static assets for Lustre UI can be found, to be
    /// served to the client.
    lustre_ui_static: String,
  )
}

pub fn html_response(html: Element(t), status: Int) -> Response {
  html
  |> element.to_document_string_builder
  |> wisp.html_response(status)
}

pub fn layout(elements: List(Element(t))) -> Element(t) {
  let body =
    list.concat([
      // Add your layout elements here!
      // Perhaps a header or navbar
      elements,
      // And then maybe a footer at the end
    ])

  html.html([], [
    html.head([], [
      html.link([
        attribute.href("/static/lustre-ui.css"),
        attribute.rel("stylesheet"),
      ]),
    ]),
    html.body([], body),
  ])
}
