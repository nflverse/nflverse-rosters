#' Build rosters from shield api
#' @param season integer: season to build
#' @return roster data frame for season
build_rosters_shieldapi <- function(season) {
  cli::cli_alert_info("Loading season rosters from Shield API")
  roster <- nflapi::nflapi_roster(season) |>
    nflapi::nflapi_roster_parse() |>
    dplyr::mutate(
      season = as.integer(season),
      player_jersey_number = as.integer(player_jersey_number),
      player_nfl_experience = as.integer(player_nfl_experience),
      player_weight = as.integer(player_weight),
      player_height = as.integer(player_height),
      player_college_names = as.character(player_college_names),
      player_headshot = gsub(
        "\\{formatInstructions\\}",
        "f_auto,q_auto",
        player_headshot
      ),
      player_birth_date = lubridate::ymd(player_birth_date)
    ) |>
    dplyr::select(
      season,
      team_abbr = team_abbreviation,
      position = player_position_group,
      depth_chart_position = player_position,
      jersey_number = player_jersey_number,
      status = player_status,
      display_name = player_display_name,
      first_name = player_first_name,
      last_name = player_last_name,
      height = player_height,
      weight = player_weight,
      gsis_id = player_gsis_id,
      years_exp = player_nfl_experience,
      headshot = player_headshot,
      birth_date = player_birth_date
    ) |>
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, ""))) |>
    dplyr::mutate_if(is.character, stringr::str_trim) |>
    dplyr::filter(!(is.na(team_abbr) & is.na(gsis_id)), first_name != "Duplicate")
  return(roster)
}
