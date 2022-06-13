most_rec_season <- stringi::stri_extract_all_regex(dir("data/seasons"), "officials_[0-9]{4}") |>
  unlist() |>
  na.omit() |>
  max() |>
  (\(x)gsub("officials_", "", x))()

most_rec_season <- ifelse(is.na(most_rec_season), 2015, most_rec_season)

cli::cli_alert_info("Fetching officials...")

if(most_rec_season > 2015) {
  scraped_game_ids <- readRDS(glue::glue("data/seasons/officials_{most_rec_season}.RDS")) |>
    dplyr::pull(old_game_id)
} else {
  scraped_game_ids <- c()
}

game_ids_to_update <- nflreadr::load_schedules(most_rec_season:nflreadr:::most_recent_season()) |>
  dplyr::pull(old_game_id) |>
  (\(x) x[which(!(x %in% scraped_game_ids))])()

if(length(game_ids_to_update)){
  cli::cli_alert_info("Scraping officials...")

  game_ids <- nflreadr::load_schedules(most_rec_season:nflreadr:::most_recent_season()) |>
    dplyr::filter(old_game_id %in% game_ids_to_update) |>
    dplyr::select(season, game_type, week, game_id, old_game_id)

  new_officials <- purrr::map_dfr(game_ids$old_game_id, \(x){
    tmp <- ngsscrapR::scrape_officials(x)
    tmp$old_game_id <- x
    tmp$game_id <- game_ids$game_id[which(game_ids$old_game_id == x)]
    tmp$season_type <- game_ids$game_type[which(game_ids$old_game_id == x)]
    tmp$season <- game_ids$season[which(game_ids$old_game_id == x)]
    tmp$week <- game_ids$week[which(game_ids$old_game_id == x)]
    if("game_key" %in% colnames(tmp)){
      tmp <- tmp[which(!is.na(tmp$game_key)),]
    }
    return(tmp)
  })

  if(most_rec_season > 2015) {
    old_officials <- readRDS(glue::glue("data/seasons/officials_{most_rec_season}.RDS"))

    officials <- old_officials |>
      dplyr::filter(!(old_game_id %in% game_ids_to_update)) |>
      dplyr::bind_rows(new_officials)
  } else {
    officials <- new_officials
  }

  purrr::walk(unique(officials$season), function(x, df){
    officials_season <- df |>
      dplyr::filter(season == x)
    saveRDS(officials_season, glue::glue("data/seasons/officials_{x}.rds"))
    readr::write_csv(officials_season, glue::glue("data/seasons/officials_{x}.csv"))
  }, officials)
} else {
  cli::cli_alert_warning("Nothing to load. It's probably offseason.")
}
