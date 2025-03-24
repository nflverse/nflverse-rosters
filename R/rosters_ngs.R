#' Build weekly rosters from NGS API
#'
#' @param season integer: season to build
#' @return dataframe of weekly rosters
build_rosters_weekly_ngsapi <- function(season) {
  cli::cli_alert_info("Using NGS API to obtain weekly roster information, scraping teams and weeks for {season}")

  teams <-
    try(ngsscrapR::scrape_teams(season) |> dplyr::filter(!is.na(division_id)))

  if (inherits(teams, "try-error")) {
    cli::cli_alert_warning("Could not pull teams from NGS for {season}. Trying previous season.")
    teams_prev <-
      try(ngsscrapR::scrape_teams(season - 1) |> dplyr::filter(!is.na(division_id)))
    if (inherits(teams_prev, "try-error")) {
      cli::cli_abort("Could not pull teams from NGS for either {season} or {season-1}. Aborting.")
    }
    teams <- teams_prev
  }

  weeks <-
    purrr::map_dfr(c("REG", "POST"), ~ ngsscrapR::scrape_schedule(season, .x)) |>
    dplyr::group_by(week, game_type) |>
    dplyr::summarise(.groups = "drop")

  team_week_pair <- teams |>
    dplyr::select(team_id) |>
    dplyr::mutate(join = 1) |>
    dplyr::left_join(weeks |>
                       dplyr::mutate(join = 1), by = c("join")) |>
    dplyr::select(-c(join)) |>
    dplyr::rename(teamId = team_id, seasonType = game_type) |>
    dplyr::mutate(
      season = !!season,
      seasonType = dplyr::case_when(seasonType != "REG" ~ "POST", T ~ seasonType)
    )
  cli::cli_alert_info("Scraping rosters for {season}...")
  scrape_rosters <- function() {
    p <- progressr::progressor(along = 1:nrow(team_week_pair))
    weekly_rosters <-
      purrr::pmap_dfr(
        list(
          team_week_pair$teamId,
          team_week_pair$season,
          team_week_pair$seasonType,
          team_week_pair$week
        ),
        \(w, x, y, z) {
          roster <- ngsscrapR::scrape_roster(w, x, y, z)
          p()
          return(roster)
        }
      )
  }

  progressr::with_progress({
    weekly_rosters <- scrape_rosters()

    if (nrow(weekly_rosters) == 0) {
      # if season roster is missing, patch with current roster and leave weeks null
      season_rosters <-
        ngsscrapR::scrape_current_roster(teamId = "ALL", season = season)
      weekly_rosters <- weekly_rosters |>
        dplyr::bind_rows(season_rosters)
    }

    weekly_rosters <- weekly_rosters |>
      dplyr::mutate(years_exp = as.integer(season) - as.integer(entry_year)) |>
      dplyr::rename(
        position = position_group,
        depth_chart_position = position,
        game_type = season_type
      ) |>
      dplyr::group_by(game_type) |>
      dplyr::mutate(week = dplyr::dense_rank(week)) |> # fixing some weirdness where we skip a week
      dplyr::ungroup() |>
      dplyr::mutate(
        game_type = dplyr::case_when(
          game_type == "POST" & week == 1 ~ "WC",
          game_type == "POST" &
            week == 2 ~ "DIV",
          game_type == "POST" &
            week == 3 ~ "CON",
          game_type == "POST" &
            week == 4 ~ "SB",
          T ~ game_type
        ),
        week = dplyr::case_when(
          game_type %in% c("WC", "DIV", "CON", "SB") ~ week + max(week[game_type == "REG"]),
          T ~ week
        ),
      )
    weekly_rosters[["birth_date"]] <-
      NULL # sometimes ngsscrapR::scrape_rosters() returns this, sometimes it doesn't
  })

  return(weekly_rosters)
}
