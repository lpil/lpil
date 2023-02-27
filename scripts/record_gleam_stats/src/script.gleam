import script/config
import script/sheets
import script/github
import script/discord
import script/hex
import gleam/io

pub fn main() {
  io.println("Loading config")
  let assert Ok(config) = config.load_from_environment()

  io.println("Querying GitHub")
  let assert Ok(cents) = github.get_estimated_monthly_income_in_cents(config)

  io.println("Querying Discord")
  let assert Ok(members) = discord.get_approximate_discord_member_count()

  io.println("Querying Hex")
  let assert Ok(#(stdlib_all, stdlib_recent)) = hex.get_stdlib_counts()

  let row =
    sheets.Row(
      monthly_sponsorship_cents: cents,
      approximate_discord_member_count: members,
      stdlib_all_downloads: stdlib_all,
      stdlib_recent_downloads: stdlib_recent,
    )

  io.println("Writing to Google Sheets")
  let assert Ok(_) = sheets.append_current_income(row, config)

  io.println("Done!")
}
