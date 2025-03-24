cli::cli_alert_info("JOB START")
pkgload::load_all()
print(sessioninfo::session_info())

season <- Sys.getenv("NFLVERSE_SEASON", unset =  nflreadr::most_recent_season(roster = TRUE))

## build most recent roster
rosters <- build_rosters(season = season)

if (nrow(rosters$weekly) == 0) {
  cli::cli_alert_danger("No weekly rosters to save")
}

if (nrow(rosters$weekly) > 0) {
  cli::cli_alert_info("Saving weekly rosters...")

  nflversedata::nflverse_save(
    data_frame = rosters$weekly,
    file_name = glue::glue("roster_weekly_{season}"),
    nflverse_type = "weekly roster data",
    release_tag = "weekly_rosters"
  )
  cli::cli_alert_success("Saved weekly rosters to nflverse-data@weekly_rosters")
}

if (nrow(rosters$season) == 0) {
  cli::cli_abort("No season rosters to save")
}

cli::cli_alert_info("Saving season rosters...")

nflversedata::nflverse_save(
  data_frame = rosters$season,
  file_name = glue::glue("roster_{season}"),
  nflverse_type = "roster data",
  release_tag = "rosters"
)

cli::cli_alert_success("Saved season rosters to nflverse-data@rosters")
cli::cli_alert_success("JOB COMPLETE")
