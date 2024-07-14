//// An adapter for the `sql` module's SQL DSL for running the queries against
//// PostgreSQL using the `gleam_pgo` library.

import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/pgo.{type Connection}
import gleam/result
import gleam/string
import sql.{type Query, type QueryError, type Value}

pub fn execute(
  query: Query,
  connection: Connection,
  parameters: List(Value),
) -> Result(List(List(Value)), QueryError) {
  let parameters = list.map(parameters, value_to_pgo)
  let sql =
    sql.to_sql(query, query_parameter_prefix: "$", escaped_double_quote: "\"")
  case pgo.execute(sql, connection, parameters, row_decoder) {
    Ok(data) -> Ok(data.rows)
    // TODO: proper errors
    Error(e) -> Error(sql.DatabaseError(string.inspect(e)))
  }
}

@external(erlang, "badminton_ffi", "coerce_tuple_to_list")
fn dynamic_tuple_to_list(in: Dynamic) -> Dynamic

fn row_decoder(
  value: dynamic.Dynamic,
) -> Result(List(Value), List(dynamic.DecodeError)) {
  let decoder =
    dynamic.list(
      of: dynamic.any([
        fn(d) { dynamic.string(d) |> result.map(sql.TextValue) },
        fn(d) { dynamic.int(d) |> result.map(sql.IntValue) },
        fn(d) { dynamic.float(d) |> result.map(sql.FloatValue) },
      ]),
    )

  decoder(dynamic_tuple_to_list(value))
}

fn value_to_pgo(value: Value) -> pgo.Value {
  case value {
    sql.TextValue(t) -> pgo.text(t)
    sql.IntValue(t) -> pgo.int(t)
    sql.FloatValue(t) -> pgo.float(t)
    sql.NullValue -> pgo.null()
  }
}
