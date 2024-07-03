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
/// 1. The search term.
/// 2. The page size.
/// 3. The page offset. If you are on the first page set it to 1.
///
///
pub fn sql_select_many_query(
  table table: String,
  id_column id_column: String,
  search_columns search_columns: List(String),
  columns columns: List(String),
) -> String {
  let search = case search_columns {
    [] -> "true"
    s ->
      string.join(list.map(s, sql_escape_name), "||") <> " ilike '%'||$1||'%'"
  }

  let parts = [
    "select",
    sql_escape_name(id_column) <> ",",
    columns |> list.map(sql_escape_name) |> string.join(", "),
    "from",
    sql_escape_name(table),
    "where",
    "($1 = '' or " <> search <> ")",
    "order by " <> sql_escape_name(id_column),
    "limit $2",
    "offset $3",
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
  id_column id_column: String,
  columns columns: List(String),
) -> String {
  let parts = [
    "select",
    sql_escape_name(id_column) <> ",",
    columns |> list.map(sql_escape_name) |> string.join(", "),
    "from",
    sql_escape_name(table),
    "where " <> sql_escape_name(id_column) <> " = $1",
    "limit 1",
  ]

  string.join(parts, " ")
}

/// Build an SQL query for updating a resource.
///
pub fn sql_update_query(
  table table: String,
  id_column id_column: String,
  columns columns: List(String),
) -> String {
  let updates =
    list.index_map(columns, fn(column, i) {
      sql_escape_name(column) <> " = $" <> int.to_string(i + 2)
    })

  let parts = [
    "update " <> sql_escape_name(table) <> " set",
    updates |> string.join(", "),
    "where " <> sql_escape_name(id_column) <> " = $1",
  ]

  string.join(parts, " ")
}

/// Build an SQL query for deleting a resource with a given id.
///
pub fn sql_delete_query(
  table table: String,
  id_column id_column: String,
) -> String {
  let parts = [
    "delete from " <> sql_escape_name(table),
    "where " <> sql_escape_name(id_column) <> " = $1",
  ]
  string.join(parts, " ")
}
