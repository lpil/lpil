import gleam/dict
import gleam/list
import gleam/string
import gsv
import simplifile

pub fn main() -> Nil {
  let assert Ok(data) = simplifile.read("data.csv") as "read"
  let assert Ok(data) = gsv.to_dicts(data, ",")
  use data <- list.each(data)

  let assert Ok(date) = dict.get(data, "Date")
  let date = date |> string.split("/") |> list.reverse |> string.join("-")
  let assert Ok("£" <> amount) = dict.get(data, "Amount")
  let assert Ok(payee) = dict.get(data, "Payee")
  let assert Ok(item) = dict.get(data, "Item")
  let notes = dict.get(data, "Notes")
  let notes = case notes {
    Ok(notes) -> "\n" <> notes <> "\n"
    _ -> ""
  }

  let filename = "out/" <> date <> " " <> payee <> " - " <> item <> ".md"
  let content = "---
date:" <> pad(date) <> "
amount:" <> pad(amount) <> "
payee:" <> pad(payee) <> "
item:" <> pad(item) <> "
---
" <> notes

  let assert Ok(_) = simplifile.write(filename, content) as "write"
}

fn pad(data: String) -> String {
  case data {
    "" -> ""
    _ -> " " <> data
  }
}
