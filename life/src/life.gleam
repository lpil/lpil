import life/config
import sturnidae
import gleam/io
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import gleam/hackney

pub fn main() {
  let config = config.load_from_environment()

  let assert Ok(response) =
    sturnidae.get_feed_items_request(
      config.starling_pat,
      config.starling_account_uid,
      config.starling_category_uid,
      "2015-01-01T01:01:00.000Z",
    )
    |> hackney.send
  let assert Ok(items) = sturnidae.get_feed_items_response(response)

  list.each(items, print_item)
  io.println("\n" <> int.to_string(list.length(items)) <> " items")
}

fn print_item(item: sturnidae.FeedItem) {
  let money = format_money(item.amount.minor_units)
  let line = string.slice(item.transaction_time, 0, 19) <> " "
  let line = line <> string.pad_left(money, 10, " ")
  let line =
    line <> case item.direction {
      sturnidae.In -> " <- "
      sturnidae.Out -> " -> "
    }
  let counter_party = string.slice(item.counter_party_name, 0, 23)
  let line = line <> string.pad_right(counter_party, 23, " ")
  let line = line <> " " <> option.unwrap(item.reference, "")
  io.println(line)
}

fn format_money(pence: Int) -> String {
  let pounds = int.to_string(pence / 100)
  let pence = string.pad_left(int.to_string(pence % 100), 2, "0")
  "£" <> pounds <> "." <> pence
}
