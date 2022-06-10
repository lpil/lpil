import script/config
import script/sheets
import script/github

pub fn main() {
  assert Ok(config) = config.load_from_environment()
  assert Ok(cents) = github.get_estimated_monthly_income_in_cents(config)
  assert Ok(_) = sheets.append_current_income(cents, config)
}
