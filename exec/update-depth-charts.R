build_dc <- function() {
  szn <- nflreadr::most_recent_season(roster = TRUE)

  new <- try(nflverse.espn::espn_depth_charts(season = szn), silent = FALSE)
  if (inherits(new, "try-error")) {
    cli::cli_abort(
      "Failed to query new depth chart data. Won't upload updates."
    )
  }

  old <- try(
    nflreadr::rds_from_url(
      glue::glue(
        "https://github.com/nflverse/nflverse-data/releases/download/",
        "depth_charts/depth_charts_{szn}.rds"
      )
    ),
    silent = FALSE
  )
  if (inherits(old, "try-error") || (nrow(old) == 0)) {
    cli::cli_abort(
      "Failed to download current depth chart data. Won't upload updates."
    )
  }

  append <- dplyr::bind_rows(old, new) |>
    dplyr::arrange(dplyr::desc(dt), team, pos_grp_id, pos_rank)

  nflversedata::nflverse_save(
    data_frame = append,
    file_name = glue::glue("depth_charts_{szn}"),
    nflverse_type = "depth charts",
    release_tag = "depth_charts"
  )
}

build_dc()
