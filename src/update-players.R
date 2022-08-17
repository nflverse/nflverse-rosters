
.players_ngs <- function(){
  cli::cli_alert_info("Scraping players from NGS...")

  player_list <- purrr::map(LETTERS, \(search_term) {
    httr::GET(
      httr::modify_url(
        "https://nextgenstats.nfl.com/api/league/player/search",
        query = list(term = search_term)
      ),
      httr::add_headers(.headers = ngsscrapR::default_headers),
      httr::set_cookies(.cookies = ngsscrapR::default_cookies)
    ) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::parse_json()
  })

  suppressWarnings({
    # this throws a warning for filling NA columns like `suffix`
    players <- player_list |>
      purrr::map("players") |>
      unlist(recursive = FALSE) |>
      data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
      dplyr::distinct(gsisId, .keep_all = TRUE) |>
      tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
      dplyr::arrange(display_name) |>
      dplyr::mutate(headshot = gsub("\\{formatInstructions\\}", "f_auto,q_auto", headshot))
  })

  return(players)
}

.parse_shitty_heights <- \(height) {
  height <- stringr::str_remove_all(height, "\\s+")
  x <- stringr::str_match(height, "([0-9]+)\\-([0-9]+)")
  suppressWarnings(as.integer(x[,2])*12 + as.integer(x[,3]))
}

.players_rosters <- function(){

  suppressWarnings({

    rosters <- nflreadr::load_rosters(TRUE) |>
      dplyr::filter(!is.na(gsis_id)) |>
      dplyr::select(
        season, gsis_id,
        full_name, first_name, last_name, birth_date, height, weight,
        college, college_conference, dplyr::contains("draft"),
        entry_year, rookie_year,
        dplyr::contains("id"), dplyr::contains("headshot"),
        -group_id, -current_team_id
      ) |>
      dplyr::group_by(gsis_id) |>
      tidyr::fill(tidyselect::everything(),.direction = "downup") |>
      dplyr::ungroup() |>
      dplyr::arrange(gsis_id, -season) |>
      dplyr::distinct(gsis_id, .keep_all = TRUE) |>
      dplyr::mutate(
        full_name = paste(first_name, last_name),
        height = ifelse(grepl(x = height, pattern = "-", fixed = TRUE),
                        .parse_shitty_heights(height),
                        as.integer(height)
        ),
        headshot = dplyr::coalesce(headshot_url, headshot),
        headshot_url = NULL,
        season = NULL
      )

  })

  return(rosters)
}

.players_otc <- function(){
  players_otc <- nflreadr::load_contracts() |>
    dplyr::select(player, position, team, otc_id, draft_year, draft_round, draft_overall, draft_team) |>
    dplyr::distinct()

  return(players_otc)
}
.players_pfr <- function(){
  # https://gist.github.com/tanho63/45031f1ffdfb4af51f2918cc6b4e9263
}

.players_override <- function(){
  players_override <- nflreadr::csv_from_url()
  return(players_override)
}

.players_ffverse <- function(){
  players_ffverse <- nflreadr::load_ff_playerids() |>
    dplyr::select(-age, -db_season)

  return(players_ffverse)
}


coalescing_join <- function(){

}

name_join <- function(){}

build_players <- function() {

  players_ngs <- .players_ngs()
  players_rosters <- .players_rosters()
  players_otc <- .players_otc()
  players_pfr <- .players_pfr()
  players_override <- .players_override()

  # do coalescing/name joins?

  cli::cli_process_start("Uploading players to nflverse-data")

  nflversedata::nflverse_save(
    data_frame = players,
    file_name = "players",
    nflverse_type = "players",
    release_tag = "players"
  )

  cli::cli_process_done()
}

build_players()
