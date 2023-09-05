build_players <- function() {
  cli::cli_alert_info("Scraping players...")

  # thank you tan lmao

  player_list <- purrr::map(LETTERS, \(search_term) {
    httr::RETRY(
      "GET",
      httr::modify_url(
        "https://nextgenstats.nfl.com/api/league/player/search",
        query = list(term = search_term)
      ),
      httr::add_headers(.headers = ngsscrapR::default_headers),
      httr::set_cookies(.cookies = ngsscrapR::default_cookies),
      pause_min = 10
    ) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::parse_json()
  },.progress = TRUE)

  suppressWarnings({
    # this throws a warning for filling NA columns like `suffix`
    players <- player_list |>
      purrr::map("players") |>
      unlist(recursive = FALSE) |>
      data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
      dplyr::distinct(gsisId, .keep_all = TRUE) |>
      tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
      dplyr::arrange(display_name) |>
      dplyr::mutate(
        headshot = gsub("\\{formatInstructions\\}", "f_auto,q_auto", headshot),
        weight = dplyr::case_when(
          weight == '' ~ NA_integer_,
          weight == 0 ~ NA_integer_,
          T ~ as.integer(weight)
        )
      ) |>
      tidyr::separate(
        height,
        into = c('feet', 'inches'),
        sep = '-',
        convert = TRUE
      ) |>
      dplyr::mutate(
        height = dplyr::case_when(!is.na(feet) & is.na(inches) ~ feet,
                                  T ~ feet * 12 + inches),
        height = dplyr::case_when(height == 0 ~ NA_integer_,
                                  T ~ height)
      ) |>
      dplyr::relocate(height,.before="weight") |>
      dplyr::select(-c(feet,inches))
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
