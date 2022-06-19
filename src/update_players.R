build_players <- function() {
  cli::cli_alert_info("Obtaining GSIS Ids to scrape...")

  player_ids <-
    nflreadr::load_depth_charts(seasons = 2001:nflreadr:::most_recent_season()) |>
    dplyr::pull(gsis_id) |>
    unique()

  scraped_players <-
    piggyback::pb_download_url(file = "players.rds",
                               repo = "nflverse/nflverse-data",
                               tag = "players") |>
    nflreadr::rds_from_url()

  if(nrow(scraped_players)) {
    player_ids <-
      player_ids[which(!(player_ids %in% scraped_players$gsis_id))]
  } else {
    cli::cli_abort("No initial player release found! Please manually build a release.")
  }

  # player_ids <- player_ids[1:2000] # because this 502's sometimes

  if (length(player_ids)) {
    cli::cli_alert_info("Scraping player IDs...")
    # hitting the api with multiple sessions :grimace: lol probably will set this to sequential to update the data object
    # future::plan(future::multisession)
    progressr::with_progress({
      p <- progressr::progressor(steps = length(player_ids))
      players <- furrr::future_map_dfr(player_ids, \(x) {
        p()
        ngsscrapR::scrape_player(x)
      })
    })
    # future::plan(future::sequential)

    cli::cli_process_start("Uploading players to nflverse-data")

    players <- dplyr::bind_rows(players,
                                scraped_players) |>
      dplyr::arrange(display_name)

    nflversedata::nflverse_save(
      data_frame = players,
      file_name = "players",
      nflverse_type = "players",
      release_tag = "players"
    )

    cli::cli_process_done()
  }
  else {
    cli::cli_alert_info("No new players found!")
  }
}

setwd(here::here())

build_players()
