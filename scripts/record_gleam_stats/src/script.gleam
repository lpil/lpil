import script/config
import script/sheets
import script/github
import script/discord
import script/exercism
import script/plausible
import script/hex
import gleam/io

pub fn main() {
  io.println("Loading config")
  let assert Ok(config) = config.load_from_environment()

  io.println("Querying GitHub")
  let assert Ok(github) = github.get_information(config)

  io.println("Querying Discord")
  let assert Ok(members) = discord.get_approximate_discord_member_count()

  io.println("Querying Hex")
  let assert Ok(#(stdlib_all, stdlib_recent)) = hex.get_stdlib_counts()

  io.println("Querying Exercism")
  let assert Ok(exercism) = exercism.get_track_information()

  io.println("Querying Plausible")
  let assert Ok(plausible) = plausible.get_stats(config)

  let row =
    sheets.Row(
      monthly_sponsorship_cents: github.estimated_monthly_sponsorship,
      sponsor_count: github.sponsor_count,
      compiler_github_stars: github.stars,
      approximate_discord_member_count: members,
      stdlib_all_downloads: stdlib_all,
      stdlib_recent_downloads: stdlib_recent,
      exercism_students_count: exercism.students_count,
      exercism_submissions_count: exercism.submissions_count,
      exercism_mentoring_discussions_count: exercism.mentoring_discussions_count,
      site_thirty_day_pageviews: plausible.thirty_day_pageviews,
      site_thirty_day_visitors: plausible.thirty_day_visitors,
    )

  io.println("Writing to Google Sheets")
  let assert Ok(_) = sheets.append_current_income(row, config)

  io.println("Done!")
}
