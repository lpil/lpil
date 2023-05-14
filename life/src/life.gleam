import life/config
import sturnidae
import gleam/hackney
import gleam/io

pub fn main() {
  let config = config.load_from_environment()

  let assert Ok(response) =
    sturnidae.get_feed_items_request(
      config.starling_pat,
      config.starling_account_uid,
      config.starling_category_uid,
      "2023-04-14T12:34:56.000Z",
    )
    |> hackney.send
}
