import html_parser.{
  Characters, EndDocument, EndElement, EndPrefixMapping, StartDocument,
  StartElement, StartPrefixMapping,
}
import gleeunit/should
import gleam/list

pub fn example1_test() {
  let html = "<html><body><h1>Hello, Joe!</h1></body></html>"
  let assert Ok(events) =
    html_parser.sax(html, [], fn(state, _, event) { [event, ..state] })

  events
  |> list.reverse
  |> should.equal([
    StartDocument,
    StartPrefixMapping("", "http://www.w3.org/1999/xhtml"),
    StartElement("http://www.w3.org/1999/xhtml", "html", #("", "html"), []),
    StartElement("http://www.w3.org/1999/xhtml", "head", #("", "head"), []),
    EndElement("http://www.w3.org/1999/xhtml", "head", #("", "head")),
    StartElement("http://www.w3.org/1999/xhtml", "body", #("", "body"), []),
    StartElement("http://www.w3.org/1999/xhtml", "h1", #("", "h1"), []),
    Characters("Hello, Joe!"),
    EndElement("http://www.w3.org/1999/xhtml", "h1", #("", "h1")),
    EndElement("http://www.w3.org/1999/xhtml", "body", #("", "body")),
    EndElement("http://www.w3.org/1999/xhtml", "html", #("", "html")),
    EndPrefixMapping(""),
    EndDocument,
  ])
}
