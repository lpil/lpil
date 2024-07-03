import badminton/query.{
  Ascending, CaseInsensitive, CaseSensitive, Delete, Descending, Equal,
  Parameter, RelationValue, Select, StringContains, Update,
}
import gleam/option.{None, Some}
import gleeunit/should

pub fn select_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("id", Descending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" order by \"id\" dsc limit 10",
  )
}

pub fn select_asc_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("id", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" order by \"id\" asc limit 10",
  )
}

pub fn select_named_table_test() {
  Select(
    from: "users",
    columns: [#(Some("users"), "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("id", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"users\".\"name\", \"email\" from \"users\" order by \"id\" asc limit 10",
  )
}

pub fn select_limit_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 1234,
    offset: 0,
    order_by: #("id", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" order by \"id\" asc limit 1234",
  )
}

pub fn select_order_column_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 1234,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" order by \"created_at\" asc limit 1234",
  )
}

pub fn select_escape_column_test() {
  Select(
    from: "users",
    columns: [#(None, "; drop table users"), #(None, "\"")],
    limit: 1234,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"; drop table users\", \"\"\"\" from \"users\" order by \"created_at\" asc limit 1234",
  )
}

pub fn select_offset_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 20,
    order_by: #("created_at", Ascending),
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" order by \"created_at\" asc limit 10 offset 20",
  )
}

pub fn select_where_equal_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [Equal(RelationValue(None, "id"), Parameter(1))],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" where \"id\" = $1 order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_sqlite_param_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [Equal(RelationValue(None, "id"), Parameter(1))],
  )
  |> query.to_sql("?", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" where \"id\" = ?1 order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_backslash_quote_escape_test() {
  Select(
    from: "users",
    columns: [#(None, "here \" there")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [],
  )
  |> query.to_sql("?", "\\\"")
  |> should.equal(
    "select \"here \\\" there\" from \"users\" order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_qualified_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [Equal(RelationValue(Some("users"), "id"), Parameter(1))],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" where \"users\".\"id\" = $1 order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_multiple_test() {
  Select(
    from: "users",
    columns: [#(None, "name"), #(None, "email")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [
      Equal(RelationValue(None, "id"), Parameter(2)),
      Equal(RelationValue(None, "is_admin"), Parameter(1)),
    ],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\", \"email\" from \"users\" where \"id\" = $2 and \"is_admin\" = $1 order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_string_contains_test() {
  Select(
    from: "users",
    columns: [#(None, "name")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [
      StringContains(RelationValue(None, "id"), Parameter(1), CaseSensitive),
    ],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\" from \"users\" where \"id\" like '%'||$1||'%' order by \"created_at\" asc limit 10",
  )
}

pub fn select_where_string_contains_insensitive_test() {
  Select(
    from: "users",
    columns: [#(None, "name")],
    limit: 10,
    offset: 0,
    order_by: #("created_at", Ascending),
    where: [
      StringContains(RelationValue(None, "id"), Parameter(1), CaseInsensitive),
    ],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "select \"name\" from \"users\" where \"id\" ilike '%'||$1||'%' order by \"created_at\" asc limit 10",
  )
}

pub fn delete_test() {
  Delete(from: "evidence", where: [])
  |> query.to_sql("$", "\"\"")
  |> should.equal("delete from \"evidence\"")
}

pub fn delete_where_test() {
  Delete(from: "evidence", where: [
    Equal(RelationValue(None, "name"), Parameter(1)),
    Equal(RelationValue(None, "size"), Parameter(2)),
    Equal(RelationValue(None, "level"), Parameter(3)),
  ])
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "delete from \"evidence\" where \"name\" = $1 and \"size\" = $2 and \"level\" = $3",
  )
}

pub fn update_test() {
  Update(
    table: "games",
    set: [#("finished", Parameter(1)), #("series", Parameter(2))],
    where: [],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal("update \"games\" set \"finished\" = $1, \"series\" = $2")
}

pub fn update_where_test() {
  Update(
    table: "games",
    set: [#("finished", Parameter(1)), #("series", Parameter(2))],
    where: [
      Equal(RelationValue(None, "name"), Parameter(3)),
      Equal(RelationValue(None, "size"), Parameter(4)),
      Equal(RelationValue(None, "level"), Parameter(5)),
    ],
  )
  |> query.to_sql("$", "\"\"")
  |> should.equal(
    "update \"games\" set \"finished\" = $1, \"series\" = $2 where \"name\" = $3 and \"size\" = $4 and \"level\" = $5",
  )
}
