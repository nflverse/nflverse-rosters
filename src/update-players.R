Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
  }, .progress = TRUE)

  suppressWarnings({
    # this throws a warning for filling NA columns like `suffix`
    players <- player_list |>
      purrr::map("players") |>
      unlist(recursive = FALSE) |>
      data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
      dplyr::distinct(gsisId, .keep_all = TRUE) |>
      tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
      dplyr::arrange(display_name) |>
      dplyr::left_join(
        nflreadr::load_rosters(T) |>
          dplyr::group_by(gsis_id) |>
          dplyr::summarise(birth_date_3 = Mode(na.omit(birth_date))),
        by = c("gsis_id")
      ) |>
      dplyr::mutate(
        headshot = gsub("\\{formatInstructions\\}", "f_auto,q_auto", headshot),
        weight = dplyr::case_when(
          weight == '' ~ NA_integer_,
          weight == 0 ~ NA_integer_,
          T ~ as.integer(weight)
        ),
        # fixing inconsistent birthday formatting
        birth_date_1 = lubridate::ymd(birth_date),
        birth_date_2 = lubridate::mdy(birth_date),
        birth_date = dplyr::coalesce(birth_date_1, birth_date_2, birth_date_3),
        birth_date = as.character(birth_date)
      ) |>
      dplyr::select(-c(birth_date_1, birth_date_2, birth_date_3)) |>
      tidyr::separate(
        height,
        fill = "left",
        into = c('feet', 'inches'),
        sep = '-',
        convert = TRUE
      ) |>
      dplyr::left_join(
        nflreadr::load_rosters(T) |>
          dplyr::group_by(gsis_id) |>
          dplyr::summarise(
            height_2 = Mode(na.omit(height)),
            weight_2 = Mode(na.omit(weight))
          ),
        by = c("gsis_id")
      ) |>
      dplyr::mutate(
        feet = dplyr::coalesce(feet, 0),
        height = feet * 12 + inches,
        height = dplyr::case_when(height == 0 ~ NA_integer_, T ~ height),
        weight = dplyr::case_when(weight == 0 ~ NA_integer_, T ~ weight),
        height = dplyr::coalesce(height, height_2),
        weight = dplyr::coalesce(weight, weight_2),
        birth_date = dplyr::case_when(birth_date == "0-03-07" ~ NA_character_, T ~ birth_date)
      ) |>
      dplyr::relocate(height, .before = "weight") |>
      dplyr::select(-c(feet, inches, height_2, weight_2, season))
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
