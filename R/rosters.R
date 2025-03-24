#' Main function for building roster files
#'
#' @param season season to build, defaults to nflreadr::most_recent_season(roster = TRUE)
#'
build_rosters <- function(season = nflreadr:::most_recent_season(roster = TRUE)) {
  cli::cli_alert_info("Building rosters for {season}")
  season <- as.numeric(season)
  df_players <- load_nflverse_players()

  weekly_rosters <- tibble::tibble()
  roster <- tibble::tibble()

  shield <- build_rosters_shieldapi(season) |> fill_ids()

  if (season < 2002) {
    cli::cli_alert_info("Season {season} is earlier than 2002, use Shield API as main source for season rosters")
    roster <- shield
  }

  if (season >= 2002 && season < 2016) {
    cli::cli_alert_info("Season {season} is between 2002 and 2015, try to use Data Exchange for weekly rosters")
    weekly_rosters <- build_rosters_weekly_dataexchange(season)
    roster <- convert_weekly_to_season_rosters(weekly_rosters)
    weekly_rosters <- weekly_rosters |>
      dplyr::select(-c(status, height, weight, headshot)) |>
      dplyr::left_join(
        shield |>
          dplyr::select(gsis_id, status, height, weight, headshot = headshot_url, birth_date) |>
          dplyr::arrange(gsis_id, status) |>
          dplyr::distinct(.keep_all = TRUE),
        by = c("gsis_id"),
        na_matches = "never"
      ) |>
      fill_ids()

    roster <- roster |>
      dplyr::select(-c(status, height, weight, headshot)) |>
      dplyr::left_join(
        shield |>
          dplyr::select(gsis_id, status, height, weight, headshot = headshot_url, birth_date) |>
          dplyr::arrange(gsis_id, status) |>
          dplyr::distinct(gsis_id, .keep_all = TRUE),
        by = c("gsis_id"),
        na_matches = "never"
      ) |>
      fill_ids()
  }

  if (season >= 2016) {
    cli::cli_alert_info("Season {season} is after 2016, try to use NGS for weekly rosters")
    weekly_rosters <- purrr::possibly(build_rosters_weekly_ngsapi, data.frame())(season)
    if (nrow(weekly_rosters) > 0) {
      roster <- convert_weekly_to_season_rosters(weekly_rosters)
      weekly_rosters <- weekly_rosters |>
        dplyr::select(-c(height)) |>
        dplyr::left_join(
          shield |>
            dplyr::select(gsis_id, birth_date, height, headshot_gsis = headshot) |>
            dplyr::distinct(),
          by = c("gsis_id"),
          na_matches = "never"
        ) |>
        dplyr::mutate(headshot = dplyr::coalesce(headshot, headshot_gsis)) |>
        dplyr::select(-c(headshot_gsis)) |>
        fill_ids()

      roster <- roster |>
        dplyr::select(-c(height)) |>
        dplyr::left_join(
          shield |>
            dplyr::select(gsis_id, birth_date, height, headshot_gsis = headshot) |>
            dplyr::distinct(),
          by = c("gsis_id"),
          na_matches = "never"
        ) |>
        dplyr::mutate(headshot = dplyr::coalesce(headshot, headshot_gsis)) |>
        dplyr::select(-c(headshot_gsis)) |>
        fill_ids()
    }
  }

  if (season >= 2016 && nrow(weekly_rosters) == 0) {
    cli::cli_alert_info("Could not retrieve weekly rosters from NGS, so using Shield as season rosters")
    roster <- shield
  }

  if (nrow(weekly_rosters) == 0) {
    cli::cli_alert_warning("No weekly rosters to return")
  }

  if (nrow(weekly_rosters) > 0) {
    cli::cli_alert_info("Processing weekly rosters")
    weekly_rosters <- weekly_rosters |>
      dplyr::mutate(
        dplyr::across(c(gsis_it_id, smart_id), as.character),
        dplyr::across(c(entry_year, rookie_year), as.integer)
      ) |>
      nflreadr:::join_coalesce(
        df_players,
        by = c("gsis_id" = "gsis_id"),
        na_matches = "never"
      )
    # fix for missing gsis_ids
    weekly_rosters_missing <- weekly_rosters |>
      dplyr::filter(is.na(gsis_id))

    if (nrow(weekly_rosters_missing)) {
      weekly_rosters_missing <- weekly_rosters_missing |>
        tibble::as_tibble() |>
        nflreadr:::join_coalesce(
          df_players,
          by = c("gsis_it_id" = "gsis_it_id"),
          na_matches = "never"
        )

      weekly_rosters <- weekly_rosters |>
        dplyr::filter(!is.na(gsis_id)) |>
        dplyr::bind_rows(weekly_rosters_missing)
    }

    weekly_rosters <- weekly_rosters |>
      dplyr::mutate(years_exp = as.integer(season) - as.integer(entry_year)) |>
      dplyr::select(dplyr::any_of(
        c(
          "season",
          "team",
          "position",
          "depth_chart_position",
          "jersey_number",
          "status",
          "full_name",
          "first_name",
          "last_name",
          "birth_date",
          "height",
          "weight",
          "college",
          "gsis_id",
          "espn_id",
          "sportradar_id",
          "yahoo_id",
          "rotowire_id",
          "pff_id",
          "pfr_id",
          "fantasy_data_id",
          "sleeper_id",
          "years_exp",
          "headshot_url",
          "ngs_position",
          "week",
          "game_type",
          "status_description_abbr",
          "football_name",
          "esb_id",
          "gsis_it_id",
          "smart_id",
          "entry_year",
          "rookie_year",
          "draft_club",
          "draft_number"
        )
      ))
    cli::cli_alert_info("Save weekly rosters...")
    nflversedata::nflverse_save(
      data_frame = weekly_rosters,
      file_name = glue::glue("roster_weekly_{season}"),
      nflverse_type = "weekly roster data",
      release_tag = "weekly_rosters"
    )
  }

  cli::cli_alert_info("Processing season rosters")

  roster <- roster |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("gsis_it_id", "smart_id")), as.character),
      dplyr::across(dplyr::any_of(c("entry_year", "rookie_year")), as.integer)
    ) |>
    nflreadr:::join_coalesce(df_players, by = c("gsis_id" = "gsis_id"), na_matches = "never")

  roster_missing <- roster |>
    dplyr::filter(is.na(gsis_id))

  if (nrow(roster_missing)) {
    roster_missing <- roster_missing |>
      tibble::as_tibble() |>
      nflreadr:::join_coalesce(df_players,
                               by = c("gsis_it_id" = "gsis_it_id"),
                               na_matches = "never"
      )
    roster <- roster |>
      dplyr::filter(!is.na(gsis_id)) |>
      dplyr::bind_rows(roster_missing)
  }
  roster <- roster |>
    dplyr::mutate(years_exp = as.integer(season) - as.integer(entry_year)) |>
    dplyr::select(
      dplyr::any_of(
        c(
          "season",
          "team",
          "position",
          "depth_chart_position",
          "jersey_number",
          "status",
          "full_name",
          "first_name",
          "last_name",
          "birth_date",
          "height",
          "weight",
          "college",
          "gsis_id",
          "espn_id",
          "sportradar_id",
          "yahoo_id",
          "rotowire_id",
          "pff_id",
          "pfr_id",
          "fantasy_data_id",
          "sleeper_id",
          "years_exp",
          "headshot_url",
          "ngs_position",
          "week",
          "game_type",
          "status_description_abbr",
          "football_name",
          "esb_id",
          "gsis_it_id",
          "smart_id",
          "entry_year",
          "rookie_year",
          "draft_club",
          "draft_number"
        )
      )
    )

  return(list(season = roster, weekly = weekly_rosters))
}
