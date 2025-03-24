#' Fill and clean IDs
#' This function applies ID cleaning and processing from
#' `nflreadr::load_players()` and `nflreadr::load_ff_playerids()`
#'
#' @param roster dataframe of roster info
#'
#' @return dataframe with filled player ids
fill_ids <- function(roster) {
  ids <- nflreadr::load_ff_playerids() |>
    dplyr::select(
      gsis_id,
      espn_id,
      yahoo_id,
      rotowire_id,
      pff_id,
      pfr_id,
      fantasy_data_id,
      sleeper_id,
      sportradar_id
    )

  roster <- roster |>
    dplyr::left_join(ids, by = c("gsis_id"), na_matches = "never") |>
    dplyr::left_join(
      nflreadr::load_players() |>
        dplyr::select(gsis_id, college = college_name) |>
        dplyr::distinct(),
      by = c("gsis_id"),
      na_matches = "never"
    )

  roster <- roster |>
    dplyr::mutate(
      headshot_url = gsub("\\{formatInstructions\\}", "f_auto,q_auto", headshot),
      position = dplyr::case_when(position == "SPEC" ~ depth_chart_position, T ~ position)
    ) |>
    dplyr::select(
      season,
      team = team_abbr,
      position,
      depth_chart_position,
      jersey_number,
      status,
      full_name = display_name,
      first_name,
      last_name,
      birth_date,
      height,
      weight,
      college,
      gsis_id,
      espn_id,
      sportradar_id,
      yahoo_id,
      rotowire_id,
      pff_id,
      pfr_id,
      fantasy_data_id,
      sleeper_id,
      years_exp,
      headshot_url,
      dplyr::any_of(
        c(
          "ngs_position",
          "week",
          "game_type",
          "status_description_abbr",
          "status_short_description",
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
    ) |>
    dplyr::mutate(
      team = dplyr::case_when(
        team == "LAR" ~ "LA",
        team == "OAK" & season >= 2020 ~ "LV",
        team == "SD" & season >= 2017 ~ "LAC",
        team == "JAC" ~ "JAX",
        team == "STL" & season >= 2016 ~ "LA",
        TRUE ~ team
      ),
      height = stringr::str_remove_all(height, "\\\""),
      height = stringr::str_replace_all(height, "'", "-"),
      height = as.integer(height),
      season = as.integer(season)
    ) |>
    dplyr::arrange(team, position)

  return(roster)
}
