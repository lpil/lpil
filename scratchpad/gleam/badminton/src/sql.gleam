import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type QueryError {
  ConstraintViolation(name: String, detail: String)
  DatabaseError(detail: String)
}

pub type Query {
  Select(
    from: String,
    columns: List(#(Option(String), String)),
    order_by: #(String, Direction),
    limit: Int,
    offset: Int,
    where: List(Condition),
  )
  Update(
    table: String,
    set: List(#(String, QueryValue)),
    where: List(Condition),
  )
  Delete(from: String, where: List(Condition))
}

pub type Value {
  TextValue(String)
  IntValue(Int)
  FloatValue(Float)
  NullValue
}

pub type QueryValue {
  RelationValue(table: Option(String), column: String)
  Parameter(number: Int)
}

pub type Condition {
  Equal(left: QueryValue, right: QueryValue)
  StringContains(
    string: QueryValue,
    substring: QueryValue,
    sensitivity: CaseSensitivity,
  )
  Or(List(Condition))
}

pub type CaseSensitivity {
  CaseSensitive
  CaseInsensitive
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
    Delete(from: table, where: conditions) -> {
      let query = "delete from " <> escape_name(table, quote)
      conditions_to_sql(query, conditions, True, param, quote)
    }

    Update(table: table, set: updates, where: conditions) -> {
      let query = "update " <> escape_name(table, quote)
      let query = updates_to_sql(query, updates, True, param, quote)
      conditions_to_sql(query, conditions, True, param, quote)
    }

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
        Descending -> query <> " desc"
      }
      let query = query <> " limit " <> int.to_string(limit)
      case offset {
        0 -> query
        _ -> query <> " offset " <> int.to_string(offset)
      }
    }
  }
}

fn updates_to_sql(
  query: String,
  updates: List(#(String, QueryValue)),
  first: Bool,
  param: String,
  quote: String,
) -> String {
  case updates {
    [] -> query
    [#(name, value), ..updates] -> {
      let query = case first {
        True -> query <> " set "
        False -> query <> ", "
      }
      let query = query <> escape_name(name, quote) <> " = "
      let query = value_to_sql(query, value, param, quote)
      updates_to_sql(query, updates, False, param, quote)
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
    Or(conditions) -> {
      list.index_fold(conditions, query, fn(query, condition, i) {
        let query = case i {
          0 -> query
          _ -> query <> " or "
        }
        condition_to_sql(query <> "(", condition, param, quote) <> ")"
      })
    }

    Equal(left, right) -> {
      let query = value_to_sql(query, left, param, quote) <> " = "
      value_to_sql(query, right, param, quote)
    }

    StringContains(left, right, sensitivity) -> {
      let query = value_to_sql(query, left, param, quote)
      let query = case sensitivity {
        CaseSensitive -> query <> " like '%'||"
        CaseInsensitive -> query <> " ilike '%'||"
      }
      value_to_sql(query, right, param, quote) <> "||'%'"
    }
  }
}

fn value_to_sql(
  query: String,
  value: QueryValue,
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
