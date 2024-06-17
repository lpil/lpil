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

pub fn sql_select_many_query_0_test() {
  internal.sql_select_many_query(table: "users", columns: [
    "name", "email", "number",
  ])
  |> should.equal(
    "select \"name\", \"email\", \"number\" from \"users\" where id > $1 order by id limit 50",
  )
}

pub fn sql_select_one_query_0_test() {
  internal.sql_select_many_query(table: "users", columns: [
    "name", "email", "number",
  ])
  |> should.equal(
    "select \"name\", \"email\", \"number\" from \"users\" where id = $1 limit 1",
  )
}
