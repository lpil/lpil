import script/config
import script/sheets
import script/github
import gleam/io

pub fn main() {
  io.println("Loading config")
  let assert Ok(config) = config.load_from_environment()

  io.println("Querying GitHub")
  let assert Ok(cents) = github.get_estimated_monthly_income_in_cents(config)
  io.println("Writing to Google Sheets")

  let assert Ok(_) =
    sheets.Row(monthly_sponsorship_cents: cents)
    |> sheets.append_current_income(config)

  io.println("Done!")
}
