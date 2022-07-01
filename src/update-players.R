build_players <- function() {
  cli::cli_alert_info("Scraping players...")

  # thank you tan lmao

  positions <-
    c(
      "QB",
      "OT",
      "DE",
      "DT",
      "LB",
      "T",
      "WR",
      "SS",
      "RB",
      "CB",
      "P",
      "K",
      "OLB",
      "G",
      "TE",
      "FS",
      "LS",
      "DB",
      "ILB",
      "C",
      "MLB",
      "NT",
      "FB",
      "OG",
      "SAF",
      "S",
      "DL",
      "HB",
      "OL",
      "KR",
      "PR"
    )

  player_list <- purrr::map(positions, \(search_term) {
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
