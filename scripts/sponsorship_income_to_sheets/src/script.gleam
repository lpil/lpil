import gleam/io
import script/config
import script/sheets

pub fn main() {
  assert Ok(config) = config.load_from_environment()
  // TODO: get current amount
  assert Ok(_) = sheets.append_current_income(103, config)
}
