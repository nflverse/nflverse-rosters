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
    release_tag = "injuries"
  )
}

# build all seasons
# seq(2009, nflreadr::most_recent_season()) |>
#   purrr::walk(build_injury_data)

# update most recent season
build_injury_data(nflreadr::most_recent_season())
