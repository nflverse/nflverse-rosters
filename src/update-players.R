#' nflverse players
#'
#' This function builds the nflverse players table. It uses `gsis_id` as primary
#' key and combines the following in priority order:
#'
#' - manual corrections (?), if available
#' - NGS API
#' - Shield API
#' - PFF
#' - ffverse player ids
#' - PFR
#' - OTC
#' - ...?
#' - existing/historical nflverse players

build_players <- function() {
  cli::cli_alert_info("Scraping players...")

  ## existing nflverse players
  nflverse_players <- nflreadr::load_players()

  ## NGS rosters
  ngs_current_rosters <- purrr::map(
    2016:nflreadr::get_current_season(roster = TRUE),
    purrr::possibly(~ngsscrapR::scrape_current_roster(teamId = "ALL", season = .x, status = "ALL"), otherwise = tibble::tibble()),
    .progress = TRUE
  ) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
    dplyr::arrange(-season) |>
    dplyr::group_by(gsis_id, display_name) |>
    tidyr::fill(-c(season, week, season_type), .direction = "downup") |>
    dplyr::distinct(gsis_id, display_name, .keep_all = TRUE)

  ## current PFF players
  pff_players <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/players_components/pff_players.rds")

  ## ffverse player ids
  ffverse_player_ids <- nflreadr::load_ff_playerids()

  ## PFR ?
  ## Shield API ? (just use rosters?)
  ## Data Exchange ? (just use rosters?)
  ## OTC ? (load_contracts (?))

  # cli::cli_process_start("Uploading players to nflverse-data")
  #
  # nflversedata::nflverse_save(
  #   data_frame = players,
  #   file_name = "players",
  #   nflverse_type = "players",
  #   release_tag = "players"
  # )
  #
  # cli::cli_process_done()

}

build_players()
