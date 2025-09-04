import log/web
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn layout(_ctx: web.RequestContext, elements: List(Element(msg))) -> String {
  html.html([], [
    html.head([], [
      html.meta([
        attribute.name("viewport"),
        attribute.content("width=device-width,initial-scale=1"),
      ]),

      html.link([
        attribute.rel("stylesheet"),
        attribute.href("/static/pico.min.css"),
      ]),
    ]),
    html.body([], [
      html.main([attribute.class("container")], elements),
      // html.script([attribute.src("/static/main.js")], ""),
    ]),
  ])
  |> element.to_document_string
}
