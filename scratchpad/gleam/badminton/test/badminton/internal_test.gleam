import badminton/internal
import gleeunit/should

pub fn sql_escape_name_0_test() {
  "name"
  |> internal.sql_escape_name
  |> should.equal("\"name\"")
}

pub fn sql_escape_name_1_test() {
  "\"name\""
  |> internal.sql_escape_name
  |> should.equal("\"\"\"name\"\"\"")
}

pub fn sql_escape_name_2_test() {
  "drop table users;"
  |> internal.sql_escape_name
  |> should.equal("\"drop table users;\"")
}

pub fn sql_escape_name_3_test() {
  "\"drop table users;\""
  |> internal.sql_escape_name
  |> should.equal("\"\"\"drop table users;\"\"\"")
}

pub fn sql_select_one_query_0_test() {
  internal.sql_select_one_query(table: "users", columns: [
    "name", "email", "number",
  ])
  |> should.equal(
    "select id, \"name\", \"email\", \"number\" from \"users\" where id = $1 limit 1",
  )
}

pub fn sql_update_query_1_test() {
  internal.sql_update_query(table: "users", columns: ["name", "email", "number"])
  |> should.equal(
    "update \"users\" set \"name\" = $2, \"email\" = $3, \"number\" = $4 where id = $1",
  )
}

pub fn sql_delete_query_1_test() {
  internal.sql_delete_query(table: "users")
  |> should.equal("delete from \"users\" where id = $1")
}

pub fn sql_select_many_query_test() {
  internal.sql_select_many_query(
    table: "users",
    search_columns: ["name", "email"],
    columns: ["name", "email", "number"],
  )
  |> should.equal(
    "select id, \"name\", \"email\", \"number\" from \"users\" where ($1 = '' or \"name\"||\"email\" ilike '%'||$1||'%') order by id limit $2 offset $3",
  )
}

pub fn sql_select_many_query_no_search_columns_test() {
  internal.sql_select_many_query(table: "users", search_columns: [], columns: [
    "name", "email", "number",
  ])
  |> should.equal(
    "select id, \"name\", \"email\", \"number\" from \"users\" where ($1 = '' or true) order by id limit $2 offset $3",
  )
}
