#' Process `load_players()` as root of players dataframe
#'
#' @return dataframe of `load_players()` output
load_nflverse_players <- function(){
  cli::cli_alert_info("Loading `players` data from nflverse")
  df_players <- nflreadr::load_players() |>
    dplyr::select(
      gsis_id,
      full_name = display_name,
      first_name,
      last_name,
      esb_id,
      birth_date,
      college = college_name,
      entry_year,
      rookie_year,
      draft_club,
      gsis_it_id,
      smart_id,
      height,
      weight
    ) |>
    dplyr::mutate(
      birth_date = as.Date(birth_date),
      dplyr::across(c(gsis_it_id, smart_id), as.character)
    )

  return(df_players)
}
