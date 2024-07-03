import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Query {
  Select(
    from: String,
    columns: List(#(Option(String), String)),
    order_by: #(String, Direction),
    limit: Int,
    offset: Int,
    where: List(Condition),
  )
}

pub type Value {
  RelationValue(table: Option(String), column: String)
  Parameter(number: Int)
}

pub type Condition {
  Equal(Value, Value)
}

pub type Direction {
  Ascending
  Descending
}

pub fn to_sql(
  query: Query,
  query_parameter_prefix param: String,
  escaped_double_quote quote: String,
) -> String {
  case query {
    Select(
      from: table,
      columns: columns,
      order_by: #(order_by_column, direction),
      limit: limit,
      offset: offset,
      where: conditions,
    ) -> {
      let query = select_columns("select ", columns, True, quote)
      let query = query <> " from " <> escape_name(table, quote)
      let query = conditions_to_sql(query, conditions, True, param, quote)
      let query = query <> " order by " <> escape_name(order_by_column, quote)
      let query = case direction {
        Ascending -> query <> " asc"
        Descending -> query <> " dsc"
      }
      let query = query <> " limit " <> int.to_string(limit)
      case offset {
        0 -> query
        _ -> query <> " offset " <> int.to_string(offset)
      }
    }
  }
}

fn conditions_to_sql(
  query: String,
  conditions: List(Condition),
  first: Bool,
  param: String,
  quote: String,
) -> String {
  case conditions {
    [] -> query
    [condition, ..conditions] -> {
      let query = case first {
        True -> query <> " where "
        False -> query <> " and "
      }
      let query = condition_to_sql(query, condition, param, quote)
      conditions_to_sql(query, conditions, False, param, quote)
    }
  }
}

fn condition_to_sql(
  query: String,
  condition: Condition,
  query_parameter_prefix param: String,
  escaped_double_quote quote: String,
) -> String {
  case condition {
    Equal(left, right) -> {
      let query = value_to_sql(query, left, param, quote) <> " = "
      value_to_sql(query, right, param, quote)
    }
  }
}

fn value_to_sql(
  query: String,
  value: Value,
  param: String,
  quote: String,
) -> String {
  case value {
    RelationValue(table: None, column: column) ->
      query <> escape_name(column, quote)

    RelationValue(table: Some(table), column: column) ->
      query <> escape_name(table, quote) <> "." <> escape_name(column, quote)

    Parameter(index) -> query <> param <> int.to_string(index)
  }
}

fn select_columns(
  query: String,
  names: List(#(Option(String), String)),
  first: Bool,
  quote: String,
) -> String {
  case names {
    [] -> query
    [#(table, column), ..names] if first -> {
      let query = select_column(query, table, column, quote)
      select_columns(query, names, False, quote)
    }
    [#(table, column), ..names] -> {
      let query = select_column(query <> ", ", table, column, quote)
      select_columns(query, names, False, quote)
    }
  }
}

fn select_column(
  query: String,
  table: Option(String),
  column: String,
  quote: String,
) -> String {
  case table {
    None -> query <> escape_name(column, quote)

    Some(table) ->
      query <> escape_name(table, quote) <> "." <> escape_name(column, quote)
  }
}

fn escape_name(name: String, quote: String) -> String {
  "\"" <> string.replace(name, "\"", quote) <> "\""
}
