import gleam/int
import gleam/list
import gleam/string

/// Escape a field name to make it suitable for use as a column or table name
/// in a query.
///
pub fn sql_escape_name(name: String) -> String {
  "\"" <> string.replace(name, "\"", "\"\"") <> "\""
}

/// Build an SQL query for a resource.
///
/// ## SQL parameters
///
/// 1. The id of the previous record, for pagination. If you are on the first
///    page set it to 0.
///
pub fn sql_select_many_query(
  table table: String,
  columns columns: List(String),
) -> String {
  let parts = [
    "select",
    columns |> list.map(sql_escape_name) |> string.join(", "),
    "from",
    sql_escape_name(table),
    "where id > $1",
    "order by id",
    "limit 50",
  ]

  string.join(parts, " ")
}

/// Build an SQL query for a resource, returning only one.
///
/// ## SQL parameters
///
/// 1. The id of the record.
///
pub fn sql_select_one_query(
  table table: String,
  columns columns: List(String),
) -> String {
  let parts = [
    "select",
    columns |> list.map(sql_escape_name) |> string.join(", "),
    "from",
    sql_escape_name(table),
    "where id = $1",
    "limit 1",
  ]

  string.join(parts, " ")
}

/// Build an SQL query for updating a resource.
///
pub fn sql_update_query(
  table table: String,
  columns columns: List(String),
) -> String {
  let updates =
    list.index_map(columns, fn(column, i) {
      sql_escape_name(column) <> " = $" <> int.to_string(i + 2)
    })

  let parts = [
    "update",
    sql_escape_name(table),
    "set",
    updates |> string.join(", "),
    "where id = $1",
  ]

  string.join(parts, " ")
}
