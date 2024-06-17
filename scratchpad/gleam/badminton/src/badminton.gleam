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
//
// https://docs.djangoproject.com/en/5.0/ref/contrib/admin/
// https://github.com/naymspace/backpex

import badminton/internal
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
    [_] -> home(request, context)
    [_, name] -> resource_list(request, name, context)
    [_, name, id] -> resource_show(request, name, id, context)
    _ -> next()
  }
}

type Context {
  Context(prefix: String, db: Connection, resources: List(Resource))
}

fn resource_show(
  request: Request,
  name: String,
  id: String,
  context: Context,
) -> Response {
  use <- wisp.require_method(request, Get)
  use resource <- require_resource(name, context.resources)
  let id = int.parse(id) |> result.unwrap(0)

  case load_row(resource, id, context) {
    Ok(row) -> resource_page(resource, id, row)
    Error(_) -> wisp.not_found()
  }
}

fn resource_page(resource: Resource, id: Int, row: List(FieldValue)) -> Response {
  [
    html.h1([], [
      element.text(resource.display_name <> ": " <> int.to_string(id)),
    ]),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn resource_list(request: Request, name: String, context: Context) -> Response {
  use <- wisp.require_method(request, Get)
  use resource <- require_resource(name, context.resources)
  let database_rows = load_rows(resource, context)

  let table_rows =
    list.map(database_rows, fn(row) {
      let row = list.zip(resource.items, row)
      html.tr(
        [],
        list.map(row, fn(row) {
          let #(definition, data) = row
          let definition = resource_item_field(definition)
          html.td([], [element.text(definition.print(data))])
        }),
      )
    })

  [
    html.h1([], [element.text(resource.display_name)]),
    html.table([], [
      html.thead([], [
        html.tr(
          [],
          list.map(resource.items, fn(i) {
            html.td([], [element.text(item_display_name(i))])
          }),
        ),
      ]),
      html.tbody([], table_rows),
    ]),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn field_storage_names(resource: Resource) -> List(String) {
  list.map(resource.items, fn(item) { resource_item_field(item).storage_name })
}

fn load_rows(resource: Resource, context: Context) -> List(List(FieldValue)) {
  let sql =
    internal.sql_select_many_query(
      resource.storage_name,
      field_storage_names(resource),
    )

  let arguments = [pgo.int(0)]
  let assert Ok(returned) = pgo.execute(sql, context.db, arguments, decode_row)
  returned.rows
}

fn load_row(
  resource: Resource,
  id: Int,
  context: Context,
) -> Result(List(FieldValue), Nil) {
  let sql =
    internal.sql_select_one_query(
      resource.storage_name,
      field_storage_names(resource),
    )

  let arguments = [pgo.int(id)]
  let assert Ok(returned) = pgo.execute(sql, context.db, arguments, decode_row)
  case returned.rows {
    [] -> Error(Nil)
    [row, ..] -> Ok(row)
  }
}

fn decode_row(data: Dynamic) -> Result(List(FieldValue), dynamic.DecodeErrors) {
  let data = dynamic_tuple_to_list(data)
  dynamic.list(decode_field_value)(data)
}

@external(erlang, "badminton_ffi", "coerce_tuple_to_list")
fn dynamic_tuple_to_list(in: Dynamic) -> Dynamic

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

fn home(request: Request, context: Context) -> Response {
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

fn resource_item_field(item: ResourceItem) -> Field {
  case item {
    ResourceField(f) -> f
    Reference(field: f, ..) -> f
  }
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
  let decoder =
    dynamic.any([
      fn(x) { dynamic.int(x) |> result.map(Int) },
      fn(x) { dynamic.string(x) |> result.map(Text) },
    ])
  decoder(dynamic)
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
