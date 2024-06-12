// TODO: non-int primary key
// TODO: primary key column not called "id"
// TODO: UI shows records for table
// TODO: pagination
// TODO: can edit records
// TODO: can create records
// TODO: datetime type
// TODO: bool type
// TODO: basic UI
// TODO: nice UI
// TODO: configure some fields to only show on single record page

import gleam/dynamic.{type Dynamic}
import gleam/http.{Get}
import gleam/http/request
import gleam/int
import gleam/list
import gleam/pgo.{type Connection}
import gleam/result
import gleam/string
import gleam/string_builder.{type StringBuilder}
import justin
import lustre/attribute
import lustre/element.{type Element as HtmlElement}
import lustre/element/html
import wisp.{type Request, type Response}

pub fn resource(storage_name: String, display_name: String) -> Resource {
  Resource(storage_name: storage_name, display_name: display_name, items: [])
}

pub fn field(resource: Resource, field: Field) -> Resource {
  Resource(..resource, items: [ResourceField(field), ..resource.items])
}

pub fn text(name: String) -> Field {
  Field(
    storage_name: name,
    display_name: justin.sentence_case(name),
    editable: False,
    print: print_text,
    parse: parse_text,
  )
}

fn parse_text(input: String) -> Result(FieldValue, String) {
  Ok(Text(input))
}

fn print_text(value: FieldValue) -> String {
  case value {
    Text(s) -> s
    // TODO: What do we do when the value type is unexpected?
    _ -> string.inspect(value)
  }
}

pub fn int(name: String) -> Field {
  Field(..text(name), print: print_int, parse: parse_int)
}

fn parse_int(input: String) -> Result(FieldValue, String) {
  case int.parse(input) {
    Ok(i) -> Ok(Int(i))
    Error(_) -> Error("must be a whole number")
  }
}

fn print_int(value: FieldValue) -> String {
  case value {
    Int(s) -> int.to_string(s)
    // TODO: What do we do when the value type is unexpected?
    _ -> string.inspect(value)
  }
}

pub fn references(
  resource: Resource,
  referenced_resource referenced_resource: String,
  referenced_field referenced_field: String,
  display_field display_field: String,
  field field: Field,
) -> Resource {
  let reference =
    Reference(
      referenced_resource: referenced_resource,
      referenced_field: referenced_field,
      display_field: display_field,
      field: field,
    )
  Resource(..resource, items: [reference, ..resource.items])
}

pub fn email(name: String) -> Field {
  Field(..text(name), parse: parse_email)
}

fn parse_email(input: String) -> Result(FieldValue, String) {
  case string.contains(input, "@") {
    True -> Ok(Text(input))
    False -> Error("must be an email")
  }
}

pub fn display(field: Field, name: String) -> Field {
  Field(..field, display_name: name)
}

pub fn editable(field: Field, is_editable: Bool) -> Field {
  Field(..field, editable: is_editable)
}

pub fn admin_console(
  request request: Request,
  db db: Connection,
  under prefix: String,
  for resources: List(Resource),
  next next: fn() -> Response,
) -> Response {
  let resources = reverse(resources, [])
  let context = Context(prefix: prefix, db: db, resources: resources)

  case request.path_segments(request) {
    [p, ..] if p != prefix -> next()
    [_] -> handle_index(request, context)
    [_, name] -> handle_resource(request, name, context)
    _ -> next()
  }
}

type Context {
  Context(prefix: String, db: Connection, resources: List(Resource))
}

fn handle_resource(request: Request, name: String, context: Context) -> Response {
  use <- wisp.require_method(request, Get)
  use resource <- require_resource(name, context.resources)

  [
    html.h1([], [element.text(resource.display_name)]),
    html.table([], [
      html.thead(
        [],
        list.map(resource.items, fn(i) {
          html.td([], [element.text(item_display_name(i))])
        }),
      ),
    ]),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn require_resource(
  name: String,
  resources: List(Resource),
  next: fn(Resource) -> Response,
) -> Response {
  let found =
    list.find(resources, fn(r) { justin.kebab_case(r.storage_name) == name })
  case found {
    Ok(resource) -> next(resource)
    _ -> wisp.not_found()
  }
}

fn handle_index(request: Request, context: Context) -> Response {
  use <- wisp.require_method(request, Get)

  [
    html.h1([], [element.text("Resources")]),
    html.ul(
      [],
      list.map(context.resources, fn(resource) {
        let href =
          context.prefix <> "/" <> justin.kebab_case(resource.storage_name)
        html.li([], [
          html.a([attribute.href(href)], [element.text(resource.display_name)]),
        ])
      }),
    ),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn page_html(elements: List(HtmlElement(_))) -> StringBuilder {
  html.html([], [html.head([], []), html.body([], elements)])
  |> element.to_string
  |> string_builder.from_string
}

pub opaque type Resource {
  Resource(
    storage_name: String,
    display_name: String,
    items: List(ResourceItem),
  )
}

type ResourceItem {
  ResourceField(Field)
  Reference(
    referenced_resource: String,
    referenced_field: String,
    display_field: String,
    field: Field,
  )
}

pub opaque type Field {
  Field(
    storage_name: String,
    display_name: String,
    editable: Bool,
    print: fn(FieldValue) -> String,
    parse: fn(String) -> Result(FieldValue, String),
  )
}

pub type FieldValue {
  Int(value: Int)
  Text(value: String)
}

pub fn decode_field_value(
  dynamic: Dynamic,
) -> Result(FieldValue, dynamic.DecodeErrors) {
  dynamic.any([
    fn(x) { dynamic.int(x) |> result.map(Int) },
    fn(x) { dynamic.string(x) |> result.map(Text) },
  ])(dynamic)
}

fn reverse(in: List(Resource), out: List(Resource)) -> List(Resource) {
  case in {
    [] -> out
    [first, ..in] ->
      reverse(in, [Resource(..first, items: list.reverse(first.items)), ..out])
  }
}

fn item_display_name(item: ResourceItem) -> String {
  case item {
    ResourceField(field) | Reference(field: field, ..) -> field.display_name
  }
}
