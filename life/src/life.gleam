import life/config
import life/generated/sql
import sturnidae
import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleam/hackney
import gleam/option.{Some}
import gleam/pgo

pub fn main() {
  let config = config.load_from_environment()
  let db =
    pgo.connect(
      pgo.Config(
        ..pgo.default_config(),
        host: "localhost",
        database: "lpil_life",
        user: "postgres",
        password: Some("postgres"),
        pool_size: 2,
      ),
    )

  let assert Ok(response) =
    sturnidae.get_feed_items_request(
      config.starling_pat,
      config.starling_account_uid,
      config.starling_category_uid,
      "2015-01-01T01:01:00.000Z",
    )
    |> hackney.send
  let assert Ok(items) = sturnidae.get_feed_items_response(response)

  // list.each(items, print_item)
  list.each(items, insert_item(_, db))
  io.println("\n" <> int.to_string(list.length(items)) <> " items")
}

fn insert_item(item: sturnidae.FeedItem, db: pgo.Connection) {
  let arguments = [
    pgo.text(item.feed_item_uid),
    pgo.int(item.amount.minor_units),
    pgo.text(item.source_amount.currency),
    pgo.int(item.source_amount.minor_units),
    pgo.text(item.updated_at),
    pgo.text(sturnidae.direction_to_string(item.direction)),
    pgo.text(item.transaction_time),
    pgo.text(item.source),
    pgo.text(sturnidae.transaction_status_to_string(item.status)),
    pgo.nullable(pgo.text, item.counter_party_type),
    pgo.nullable(pgo.text, item.counter_party_uid),
    pgo.text(item.counter_party_name),
    pgo.nullable(pgo.text, item.reference),
    pgo.text(item.country),
    pgo.text(item.spending_category),
    pgo.nullable(pgo.text, item.user_note),
  ]

  let assert Ok(_) = sql.upsert_starling_transaction(db, arguments, Ok)
  Nil
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
