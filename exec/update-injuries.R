build_injury_data <- function(season) {
  cli::cli_progress_step(
    "Query {.val {season}} injury data"
  )

  data <- nflapi::nflapi_injuries(season = season)

  cli::cli_progress_step(
    "Upload {.val {season}} injury data"
  )

  nflversedata::nflverse_save(
    data_frame = data,
    file_name = paste0("injuries_", season),
    nflverse_type = "injury & practice reports",
    release_tag = "injuries",
    timestamp = upload_timestamp
  )
}

# The workflow uploads the release timestamps once per run in a final job
upload_timestamp <- !identical(tolower(Sys.getenv("NFLVERSE_UPLOAD_TIMESTAMPS", "true")), "false")

# a single season (2009 or later), defaults to the most recent one
build_injury_data(
  as.integer(Sys.getenv("NFLVERSE_UPDATE_SEASON", unset = nflreadr::most_recent_season(roster = TRUE)))
)
