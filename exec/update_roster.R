cli::cli_alert_info("JOB START")
pkgload::load_all()
print(sessionInfo())

season <- Sys.getenv("NFLVERSE_UPDATE_SEASON", unset = nflreadr::most_recent_season(roster = TRUE))
# The workflow uploads the release timestamps once per run in a final job
upload_timestamp <- !identical(tolower(Sys.getenv("NFLVERSE_UPLOAD_TIMESTAMPS", "true")), "false")

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
    release_tag = "weekly_rosters",
    timestamp = upload_timestamp
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
  release_tag = "rosters",
  timestamp = upload_timestamp
)

cli::cli_alert_success("Saved season rosters to nflverse-data@rosters")
cli::cli_alert_success("JOB COMPLETE")
