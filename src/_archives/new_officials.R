game_ids <- nflreadr::load_schedules(2015:nflreadr:::most_recent_season()) |>
  dplyr::select(season, game_type, week, game_id, old_game_id)

officials <- purrr::map_dfr(game_ids$old_game_id, \(x){
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

purrr::walk(unique(officials$season), function(x, df){
  officials_season <- df |>
    dplyr::filter(season == x)
  saveRDS(officials_season, glue::glue("data/seasons/officials_{x}.rds"))
  readr::write_csv(officials_season, glue::glue("data/seasons/officials_{x}.csv"))
}, officials)
