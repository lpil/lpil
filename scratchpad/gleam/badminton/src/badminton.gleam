// TODO: non-int primary key
// TODO: primary key column not called "id"
// TODO: can create records
// TODO: datetime type
// TODO: bool type
// TODO: basic UI
// TODO: nice UI
// TODO: configure some fields to only show on single record page
// TODO: do not use relative URLs
// TODO: tests for home
// TODO: tests for resource list rows
// TODO: tests for resource list rows with no searchable
// TODO: tests for resource list rows with search
// TODO: tests for resource show one
// TODO: tests for resource update one
// TODO: tests for resource delete one
// TODO: test pagination
// TODO: test pagination (invalid page number, 0 or negative)
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
import gleam/uri
import justin
import lustre/attribute
import lustre/element.{type Element as HtmlElement}
import lustre/element/html
import wisp.{type Request, type Response}

pub fn resource(storage_name: String, display_name: String) -> Resource {
  Resource(
    storage_name: storage_name,
    display_name: display_name,
    id_field: Field(
      storage_name: "id",
      display_name: "id",
      editable: False,
      searchable: False,
      print: print_int,
      parse: parse_int,
    ),
    fields: [],
    deletable: False,
  )
}

pub fn field(resource: Resource, field: Field) -> Resource {
  Resource(..resource, fields: [ResourceField(field), ..resource.fields])
}

pub fn text(name: String) -> Field {
  Field(
    storage_name: name,
    display_name: justin.sentence_case(name),
    editable: False,
    searchable: False,
    print: print_text,
    parse: parse_text,
  )
}

pub fn deletable(resource: Resource, is_deletable: Bool) -> Resource {
  Resource(..resource, deletable: is_deletable)
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
  Resource(..resource, fields: [reference, ..resource.fields])
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

pub fn searchable(field: Field, is_searchable: Bool) -> Field {
  Field(..field, searchable: is_searchable)
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
    [_, name, id] -> resource_single(request, name, id, context)
    _ -> next()
  }
}

type Context {
  Context(prefix: String, db: Connection, resources: List(Resource))
}

fn resource_single(
  request: Request,
  name: String,
  id: String,
  context: Context,
) -> Response {
  use resource <- require_resource(name, context.resources)
  let id = int.parse(id) |> result.unwrap(0)

  case request.method {
    http.Get -> resource_show(resource, id, context)
    http.Post -> resource_update(request, resource, id, context)
    http.Delete -> resource_delete(resource, id, context)
    _ -> wisp.method_not_allowed([http.Get, http.Post])
  }
}

fn resource_delete(resource: Resource, id: Int, context: Context) -> Response {
  let sql =
    internal.sql_delete_query(
      resource.storage_name,
      resource.id_field.storage_name,
    )
  let assert Ok(_) = pgo.execute(sql, context.db, [pgo.int(id)], Ok)
  wisp.redirect(".")
}

fn resource_update(
  request: Request,
  resource: Resource,
  id: Int,
  context: Context,
) -> Response {
  use form <- wisp.require_form(request)

  // TODO: handle the case where parsing fails due to bad input
  let assert Ok(data) = parse_form_values(form, resource)

  let #(fields, values) = list.unzip(data)
  let sql =
    internal.sql_update_query(
      resource.storage_name,
      resource.id_field.storage_name,
      fields,
    )
  let arguments = [pgo.int(id), ..list.map(values, field_value_to_pgo_value)]
  let assert Ok(_) = pgo.execute(sql, context.db, arguments, Ok)

  wisp.redirect(int.to_string(id))
}

fn field_value_to_pgo_value(value: FieldValue) -> pgo.Value {
  case value {
    Text(t) -> pgo.text(t)
    Int(i) -> pgo.int(i)
  }
}

fn parse_form_values(
  form: wisp.FormData,
  resource: Resource,
) -> Result(List(#(String, FieldValue)), String) {
  let fields = editable_fields(resource)
  list.try_map(fields, fn(field) {
    let value =
      form.values |> list.key_find(field.storage_name) |> result.unwrap("")
    case field.parse(value) {
      Ok(value) -> Ok(#(field.storage_name, value))
      Error(e) -> Error(e)
    }
  })
}

fn editable_fields(resource: Resource) -> List(Field) {
  resource.fields
  |> list.map(resource_item_field)
  |> list.filter(fn(f) { f.editable })
}

fn resource_show(resource: Resource, id: Int, context: Context) -> Response {
  case load_row(resource, id, context) {
    Ok(row) -> resource_page(resource, id, row, context)
    Error(_) -> wisp.not_found()
  }
}

fn field_input(field: Field, value: FieldValue) -> element.Element(a) {
  html.input([
    attribute.name(field.storage_name),
    attribute.value(field.print(value)),
    attribute.readonly(!field.editable),
  ])
}

fn resource_page(
  resource: Resource,
  id: Int,
  row: Row,
  context: Context,
) -> Response {
  let row = list.zip(resource.fields, row.values)
  let fields =
    list.map(row, fn(row) {
      let #(definition, value) = row
      let field = resource_item_field(definition)

      html.label(
        [attribute.style([#("display", "block"), #("padding-bottom", "10px")])],
        [
          html.div([], [element.text(field.display_name)]),
          field_input(field, value),
        ],
      )
    })

  [
    html.h1([], [
      element.text(resource.display_name <> ": " <> int.to_string(id)),
    ]),
    html.form([attribute.method("POST")], [
      element.fragment(fields),
      html.input([attribute.type_("submit"), attribute.value("Save")]),
    ]),
    html.form([attribute.method("POST"), attribute.action("?_method=DELETE")], [
      html.input([attribute.type_("submit"), attribute.value("Delete")]),
    ]),
    html.a(
      [attribute.href("/" <> context.prefix <> "/" <> resource.storage_name)],
      [element.text("Back")],
    ),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn searchable_fields(resource: Resource) -> List(String) {
  list.filter_map(resource.fields, fn(item) {
    case resource_item_field(item) {
      Field(searchable: True, storage_name: name, ..) -> Ok(name)
      _ -> Error(Nil)
    }
  })
}

fn resource_list(request: Request, name: String, context: Context) -> Response {
  use <- wisp.require_method(request, Get)
  use resource <- require_resource(name, context.resources)
  let query = wisp.get_query(request)
  let page =
    query
    |> list.key_find("after")
    |> result.try(int.parse)
    |> result.unwrap(1)
    |> int.max(1)
  let search = query |> list.key_find("search") |> result.unwrap("")

  let searchable = searchable_fields(resource)

  let search_section = case searchable {
    [] -> element.none()
    _ ->
      html.search([], [
        html.form([], [
          html.input([
            attribute.type_("search"),
            attribute.name("search"),
            attribute.value(search),
          ]),
          html.input([attribute.type_("submit"), attribute.value("Search")]),
        ]),
      ])
  }

  let database_rows = load_rows(resource, context, search: search, page: page)
  let params = []
  let params = case search {
    "" -> params
    _ -> [#("search", search), ..params]
  }
  let prev_params = [#("after", int.to_string(page - 1)), ..params]
  let next_params = [#("after", int.to_string(page + 1)), ..params]
  let prev_url = name <> "?" <> uri.query_to_string(prev_params)
  let next_url = name <> "?" <> uri.query_to_string(next_params)

  let table_rows =
    list.map(database_rows, fn(row) {
      let url = name <> "/" <> int.to_string(row.id)
      let row = list.zip(resource.fields, row.values)
      let values =
        list.map(row, fn(row) {
          let #(definition, data) = row
          let definition = resource_item_field(definition)
          html.td([], [element.text(definition.print(data))])
        })
      html.tr([], [
        element.fragment(values),
        html.td([], [html.a([attribute.href(url)], [element.text("View")])]),
      ])
    })

  [
    html.h1([], [element.text(resource.display_name)]),
    search_section,
    html.table([], [
      html.thead([], [
        html.tr(
          [],
          list.map(resource.fields, fn(i) {
            html.td([], [element.text(item_display_name(i))])
          }),
        ),
      ]),
      html.tbody([], table_rows),
    ]),
    html.nav([], [
      html_when(
        page > 1,
        html.a([attribute.href(prev_url)], [element.text("Previous")]),
      ),
      html_when(
        table_rows != [],
        html.a([attribute.href(next_url)], [element.text("Next")]),
      ),
    ]),
    html.a([attribute.href("/" <> context.prefix)], [element.text("Back")]),
  ]
  |> page_html
  |> wisp.html_response(200)
}

fn html_when(condition: Bool, element: element.Element(a)) -> element.Element(a) {
  case condition {
    True -> element
    False -> element.none()
  }
}

fn field_storage_names(resource: Resource) -> List(String) {
  list.map(resource.fields, fn(item) { resource_item_field(item).storage_name })
}

fn load_rows(
  resource: Resource,
  context: Context,
  search search: String,
  page page: Int,
) -> List(Row) {
  let page_size = 50
  let offset = { page - 1 } * page_size
  let sql =
    internal.sql_select_many_query(
      resource.storage_name,
      resource.id_field.storage_name,
      searchable_fields(resource),
      field_storage_names(resource),
    )

  let arguments = [pgo.text(search), pgo.int(page_size), pgo.int(offset)]
  let assert Ok(returned) = pgo.execute(sql, context.db, arguments, decode_row)
  returned.rows
}

fn load_row(resource: Resource, id: Int, context: Context) -> Result(Row, Nil) {
  let sql =
    internal.sql_select_one_query(
      resource.storage_name,
      resource.id_field.storage_name,
      field_storage_names(resource),
    )

  let arguments = [pgo.int(id)]
  let assert Ok(returned) = pgo.execute(sql, context.db, arguments, decode_row)
  case returned.rows {
    [] -> Error(Nil)
    [row, ..] -> Ok(row)
  }
}

fn decode_row(data: Dynamic) -> Result(Row, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode2(Row, dynamic.element(0, dynamic.int), fn(data) {
      let data = dynamic_tuple_to_list(data)
      dynamic.list(decode_field_value)(data)
      |> result.map(list.drop(_, 1))
    })
  decoder(data)
}

@external(erlang, "badminton_ffi", "coerce_tuple_to_list")
fn dynamic_tuple_to_list(in: Dynamic) -> Dynamic

fn require_resource(
  name: String,
  resources: List(Resource),
  next: fn(Resource) -> Response,
) -> Response {
  let found = list.find(resources, fn(r) { r.storage_name == name })
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
        let href = "/" <> context.prefix <> "/" <> resource.storage_name
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
    id_field: Field,
    fields: List(ResourceItem),
    deletable: Bool,
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

type Row {
  Row(id: Int, values: List(FieldValue))
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
    searchable: Bool,
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
      reverse(in, [Resource(..first, fields: list.reverse(first.fields)), ..out])
  }
}

fn item_display_name(item: ResourceItem) -> String {
  case item {
    ResourceField(field) | Reference(field: field, ..) -> field.display_name
  }
}
