import gleam/int
import gleam/pgo.{type Connection}
import gleam/string
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
    display_name: string.capitalise(name),
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
  under path: String,
  for resources: List(Resource),
  next next: fn() -> Response,
) -> Response {
  todo
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
