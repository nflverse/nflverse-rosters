build_dc <- function() {
  szn <- nflreadr::most_recent_season(roster = TRUE)

  new <- try(nflverse.espn::espn_depth_charts(season = szn), silent = FALSE)
  if (inherits(new, "try-error") || (nrow(new) == 0)) {
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

  # we use the espn_id <-> gsis_id map from nflverse-players to join as many
  # gsis_ids as we can
  gsis_map <- try(nflverse.players::players_download("full"), silent = FALSE)
  if (inherits(gsis_map, "try-error") || (nrow(gsis_map) == 0)) {
    cli::cli_abort(
      "Failed to download gsis id mapping. Won't upload updates."
    )
  }
  gsis_map <- setNames(gsis_map$gsis_id, gsis_map$espn_id)

  prepend <- dplyr::bind_rows(new, old) |>
    dplyr::arrange(dplyr::desc(dt), team, pos_grp_id, pos_rank) |>
    # we already do this in nflverse.espn::espn_depth_charts but we need
    # to apply gsis_id updates to all data.
    # Also true for the pos_name strings
    dplyr::mutate(
      gsis_id = unname(gsis_map[as.character(espn_id)]),
      pos_name = stringr::str_squish(pos_name)
    ) |>
    # clean player names a bit. Too lazy to write it out so I use the helper
    # from nflverse.espn
    nflverse.espn:::.espn_replace_na_names()

  nflversedata::nflverse_save(
    data_frame = prepend,
    file_name = glue::glue("depth_charts_{szn}"),
    nflverse_type = "depth charts",
    release_tag = "depth_charts"
  )
}

build_dc()
