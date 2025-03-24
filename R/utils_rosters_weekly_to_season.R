convert_weekly_to_season_rosters <- function(weekly_rosters) {
  roster <- weekly_rosters |>
    dplyr::group_by(season, week, gsis_id) |>
    dplyr::mutate(group_id = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::group_by(season, gsis_id) |>
    dplyr::filter(group_id == max(group_id)) |>
    dplyr::ungroup() |>
    dplyr::mutate(season = as.integer(season), week = as.integer(week))
  return(roster)
}
