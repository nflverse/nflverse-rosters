scrape_officials <-
  function() {
    ngs_schedule_iter <-
      expand.grid(
        season = 2015:(nflreadr:::most_recent_season() + 1),
        seasonType = c("REG", "POST")
      )

    ngs_schedule <-
      purrr::map2_dfr(
        ngs_schedule_iter$season,
        as.character(ngs_schedule_iter$seasonType),
        ngsscrapR::scrape_schedule
      )
    completed_games <-
      piggyback::pb_download_url(file = "officials.csv",
                                 repo = "nflverse/nflverse-data",
                                 tag = "officials") |>
      read.csv()

    game_ids <- ngs_schedule |>
      dplyr::filter(!(game_key %in% completed_games$game_key)) |>
      dplyr::select(game_key)

    if (!nrow(game_ids)) {
      cli::cli_alert_info("No new games to scrape!")
      return(F)
    }

    cli::cli_alert_info("Scraping officials...")

    new_officials <- purrr::map_dfr(game_ids$game_key, \(x) {
      payload <- ngsscrapR::scrape_officials(gameKey = x)
      if ("game_key" %in% colnames(payload)) {
        payload <- payload[which(!is.na(payload$game_key)),]
      }
      return(payload)
    })

    old_officials <- nflreadr::rds_from_url(
      "https://github.com/nflverse/nflverse-data/releases/download/officials/officials.RDS"
    )

    new_officials <-
      dplyr::bind_rows(old_officials, new_officials) |>
      dplyr::arrange(game_id)

    # some officials are missing season/season_type/week, this fills them in though not in an nflreadr friendly way
    new_officials <- new_officials |>
      dplyr::select(-c(season, season_type, week)) |>
      dplyr::inner_join(
        ngs_schedule |>
          dplyr::mutate(game_id = as.character(game_id)) |>
          dplyr::select(game_id, season, season_type = game_type, week),
        by = c("game_id")
      ) |>
      dplyr::arrange(game_id)

    cli::cli_process_start("Uploading officials to nflverse-data")

    nflversedata::nflverse_save(
      data_frame = new_officials,
      file_name = "officials",
      nflverse_type = "officials",
      release_tag = "officials"
    )

    cli::cli_process_done()
  }

scrape_officials()
