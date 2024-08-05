// TODO: non-int primary key
// TODO: primary key column not called "id"
// TODO: can create records
// TODO: datetime type
// TODO: bool type
// TODO: configure some fields to only show on single record page
// TODO: tests for home
// TODO: tests for resource list rows
// TODO: tests for resource list rows with no searchable
// TODO: tests for resource list rows with search
// TODO: tests for resource show one
// TODO: tests for resource update one
// TODO: tests for resource delete one
// TODO: test pagination
// TODO: test pagination (invalid page number, 0 or negative)

import badminton/internal
import gleam/dynamic.{type Dynamic}
import gleam/erlang
import gleam/http.{Get}
import gleam/http/request
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import gleam/string_builder.{type StringBuilder}
import gleam/uri
import justin
import lustre/attribute
import lustre/element.{type Element as HtmlElement}
import lustre/element/html
import sql
import wisp.{type Request, type Response}

const page_size = 50

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

pub fn handle_request(
  request request: Request,
  path path: List(String),
  execute_query execute_query: fn(sql.Query, List(sql.Value)) ->
    Result(List(List(sql.Value)), sql.QueryError),
  for resources: List(Resource),
) -> Response {
  use <- remove_trailing_slash(request)

  let resources = reverse(resources, [])
  let context =
    Context(
      request: request,
      execute_query: convert_query_executor(execute_query),
      resources: resources,
    )

  case path {
    [] -> home(request, context)
    ["script.js"] -> asset("script.js")
    ["styles.css"] -> asset("styles.css")
    ["pico.min.css"] -> asset("pico.min.css")
    [name] -> resource_list(request, name, context)
    [name, id] -> resource_single(request, name, id, context)
    _ -> wisp.not_found()
  }
}

fn remove_trailing_slash(request: Request, next: fn() -> Response) -> Response {
  case string.ends_with(request.path, "/") {
    True -> wisp.redirect(to: string.drop_right(request.path, 1))
    False -> next()
  }
}

type Context {
  Context(
    request: Request,
    execute_query: fn(sql.Query, List(sql.Value)) ->
      Result(List(Row), sql.QueryError),
    resources: List(Resource),
  )
}

fn asset(file: String) -> Response {
  let assert Ok(dir) = erlang.priv_directory("badminton")
  wisp.ok()
  |> wisp.set_body(wisp.File(dir <> "/static/" <> file))
  |> wisp.set_header("content-type", "text/css; charset=utf-8")
  // TODO: add cache headers
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
  let query =
    sql.Delete(from: resource.storage_name, where: [
      sql.Equal(sql.RelationValue(option.None, "id"), sql.Parameter(1)),
    ])
  let assert Ok(_) = context.execute_query(query, [sql.IntValue(id)])
  wisp.redirect("../" <> resource.storage_name)
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

  let query =
    sql.Update(
      table: resource.storage_name,
      set: list.index_map(fields, fn(field, i) {
        #(field, sql.Parameter(i + 2))
      }),
      where: [sql.Equal(sql.RelationValue(option.None, "id"), sql.Parameter(1))],
    )

  let assert Ok(_) =
    context.execute_query(query, [
      sql.IntValue(id),
      ..list.map(values, field_value_to_sql_value)
    ])
  wisp.redirect(int.to_string(id))
}

fn field_value_to_sql_value(value: FieldValue) -> sql.Value {
  case value {
    Text(t) -> sql.TextValue(t)
    Int(i) -> sql.IntValue(i)
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

  let location =
    ResourceShow(
      resource_name: resource.display_name,
      resource_slug: resource.storage_name,
      instance_name: int.to_string(id),
    )

  [
    html.form([attribute.method("POST")], [
      element.fragment(fields),
      html.input([attribute.type_("submit"), attribute.value("Save")]),
    ]),
    html.form(
      [
        attribute.method("POST"),
        attribute.action(
          resource.storage_name <> "/" <> int.to_string(id) <> "?_method=DELETE",
        ),
      ],
      [
        html.input([
          attribute.class("secondary"),
          attribute.type_("submit"),
          attribute.value("Delete"),
        ]),
      ],
    ),
  ]
  |> page_html(context: context, location: location)
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
          html.fieldset([attribute.role("group")], [
            html.input([
              attribute.type_("search"),
              attribute.name("search"),
              attribute.value(search),
            ]),
            html.input([attribute.type_("submit"), attribute.value("Search")]),
          ]),
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
        html.td([], [html.a([attribute.href(url)], [element.text("Edit")])]),
      ])
    })

  [
    search_section,
    html.div([attribute.class("overflow-auto")], [
      html.table([attribute.class("overflow-auto")], [
        html.thead([], [
          html.tr([], [
            element.fragment(
              list.map(resource.fields, fn(i) {
                html.td([], [element.text(item_display_name(i))])
              }),
            ),
            html.td([], []),
          ]),
        ]),
        html.tbody([], table_rows),
      ]),
    ]),
    html.nav([attribute.class("pagination")], [
      case page > 1 {
        True -> html.a([attribute.href(prev_url)], [element.text("Previous")])
        False -> html.span([], [element.text("Previous")])
      },
      case list.length(table_rows) == page_size {
        True -> html.a([attribute.href(next_url)], [element.text("Next")])
        False -> html.span([], [element.text("Next")])
      },
    ]),
  ]
  |> page_html(
    context: context,
    location: ResourceList(
      resource_name: resource.display_name,
      resource_slug: resource.storage_name,
    ),
  )
  |> wisp.html_response(200)
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
  let offset = { page - 1 } * page_size
  let sql =
    internal.sql_select_many_query(
      resource.storage_name,
      resource.id_field.storage_name,
      searchable_fields(resource),
      field_storage_names(resource),
    )

  let #(where, parameters) = case search {
    "" -> #([], [])
    _ -> {
      let conditions =
        list.map(searchable_fields(resource), fn(field_name) {
          sql.StringContains(
            string: sql.RelationValue(option.None, field_name),
            substring: sql.Parameter(1),
            sensitivity: sql.CaseInsensitive,
          )
        })
      #([sql.Or(conditions)], [sql.TextValue(search)])
    }
  }

  let query =
    sql.Select(
      from: resource.storage_name,
      columns: select_columns(resource),
      order_by: #("id", sql.Descending),
      limit: page_size,
      offset: offset,
      where: where,
    )

  let assert Ok(rows) = context.execute_query(query, parameters)
  rows
}

fn select_columns(resource: Resource) -> List(#(Option(String), String)) {
  [
    #(option.None, "id"),
    ..list.map(resource.fields, fn(field) {
      #(option.None, resource_item_field(field).storage_name)
    })
  ]
}

fn load_row(resource: Resource, id: Int, context: Context) -> Result(Row, Nil) {
  let query =
    sql.Select(
      from: resource.storage_name,
      columns: select_columns(resource),
      order_by: #("id", sql.Descending),
      limit: 1,
      offset: 0,
      where: [sql.Equal(sql.RelationValue(option.None, "id"), sql.Parameter(1))],
    )

  let assert Ok(rows) = context.execute_query(query, [sql.IntValue(id)])
  case rows {
    [] -> Error(Nil)
    [row, ..] -> Ok(row)
  }
}

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
    html.ul(
      [],
      list.map(context.resources, fn(resource) {
        html.li([], [
          html.a([attribute.href(resource.storage_name)], [
            element.text(resource.display_name),
          ]),
        ])
      }),
    ),
  ]
  |> page_html(context: context, location: Home)
  |> wisp.html_response(200)
}

pub type Location {
  Home
  ResourceList(resource_name: String, resource_slug: String)
  ResourceShow(
    resource_name: String,
    resource_slug: String,
    instance_name: String,
  )
}

fn breadcrumbs(location: Location) -> element.Element(a) {
  let crumbs = case location {
    Home -> [html.li([], [html.text("Home")])]

    ResourceList(resource_name:, ..) -> [
      html.li([], [html.a([attribute.href(".")], [html.text("Home")])]),
      html.li([], [html.text(resource_name)]),
    ]

    ResourceShow(resource_name:, resource_slug:, instance_name:) -> [
      html.li([], [html.a([attribute.href(".")], [html.text("Home")])]),
      html.li([], [
        html.a([attribute.href(resource_slug)], [html.text(resource_name)]),
      ]),
      html.li([], [html.text(instance_name)]),
    ]
  }

  html.div([], [
    html.a([attribute.attribute("data-hamburger", "")], [html.text("🍔")]),
    html.nav([attribute.attribute("aria-label", "breadcrumb")], [
      html.ul([], crumbs),
    ]),
  ])
}

fn page_html(
  html elements: List(HtmlElement(_)),
  location location: Location,
  context context: Context,
) -> StringBuilder {
  let nav =
    html.nav([attribute.class("sidebar"), attribute.class("container-fluid")], [
      html.ul([], [
        html.li([], [html.h1([], [element.text("Admin")])]),
        ..list.map(context.resources, fn(resource) {
          html.li([], [
            html.a([attribute.href(resource.storage_name)], [
              element.text(resource.display_name),
            ]),
          ])
        })
      ]),
    ])

  html.html([], [
    html.head([], [
      html.meta([attribute.attribute("charset", "UTF-8")]),
      html.meta([
        attribute.name("viewport"),
        attribute.attribute("content", "width=device-width, initial-scale=1"),
      ]),
      html.base([attribute.href(base_path(context.request, location))]),
      html.link([attribute.rel("stylesheet"), attribute.href("pico.min.css")]),
      html.link([attribute.rel("stylesheet"), attribute.href("styles.css")]),
    ]),
    html.body([], [
      nav,
      html.main([attribute.class("container-fluid")], [
        breadcrumbs(location),
        ..elements
      ]),
    ]),
    html.script([attribute.src("script.js")], ""),
  ])
  |> element.to_string
  |> string.append("<!doctype html>", _)
  |> string_builder.from_string
}

fn base_path(request: Request, location: Location) -> String {
  let get = fn(amount) {
    request.path_segments(request)
    |> list.reverse
    |> list.drop(amount)
    |> list.reverse
    |> string.join("/")
    |> string.append("/")
    |> string.append("/", _)
  }
  case location {
    Home -> request.path <> "/"
    ResourceList(..) -> get(1)
    ResourceShow(..) -> get(2)
  }
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

fn convert_query_executor(
  executor: fn(sql.Query, List(sql.Value)) ->
    Result(List(List(sql.Value)), sql.QueryError),
) -> fn(sql.Query, List(sql.Value)) -> Result(List(Row), sql.QueryError) {
  fn(query, parameters) {
    use rows <- result.map(executor(query, parameters))
    use row <- list.map(rows)
    case row {
      [] -> panic as "row was empty, no id"
      [sql.IntValue(id), ..row] ->
        Row(id: id, values: list.map(row, sql_value_to_field_value))
      [_, ..] -> panic as "unexpected id type, not an int"
    }
  }
}

fn sql_value_to_field_value(value: sql.Value) -> FieldValue {
  case value {
    sql.IntValue(i) -> Int(i)
    sql.TextValue(i) -> Text(i)
    sql.FloatValue(_) -> panic as "float values not yet supported"
    sql.NullValue -> panic as "null values not yet supported"
  }
}
