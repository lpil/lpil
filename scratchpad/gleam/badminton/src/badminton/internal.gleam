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
pub fn sql_select_query(
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
