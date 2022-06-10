import script/config
import script/sheets
import script/github
import gleam/io

pub fn main() {
  io.println("Loading config")
  assert Ok(config) = config.load_from_environment()
  io.println("Querying GitHub")
  assert Ok(cents) = github.get_estimated_monthly_income_in_cents(config)
  io.println("Writing to Google Sheets")
  assert Ok(_) = sheets.append_current_income(cents, config)
  io.println("Done!")
}
