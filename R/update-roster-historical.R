seasons_to_scrape <- nflreadr::load_schedules() |>
  dplyr::pull(season) |>
  unique()

valid_weeks <- nflreadr::load_schedules() |>
  dplyr::mutate(game_type = dplyr::case_when(game_type == 'REG' ~ 'REG',
                                             game_type %in% c('WC','DIV','CON','SB') ~ 'POST',
                                             T~NA_character_)) |>
  dplyr::filter(!is.na(game_type)) |>
  dplyr::group_by(season, game_type) |>
  dplyr::mutate(week = as.numeric(as.factor(week))) |>
  dplyr::ungroup() |>
  dplyr::group_by(season, game_type, week) |>
  dplyr::summarise(games_played = sum(!is.na(result))) |>
  dplyr::ungroup() |>
  dplyr::mutate(filter_week = which(games_played == 0)[1]) |>
  dplyr::filter(dplyr::row_number() <= filter_week) |>
  dplyr::select(c(season, game_type = game_type, week))

postseason_teams <- nflreadr::load_schedules() |>
  dplyr::mutate(game_type = dplyr::case_when(game_type == 'REG' ~ 'REG',
                                             game_type %in% c('WC','DIV','CON','SB') ~ 'POST',
                                             T~NA_character_)) |>
  dplyr::filter(!is.na(game_type)) |>
  dplyr::group_by(season, game_type) |>
  dplyr::mutate(week = as.numeric(as.factor(week))) |>
  dplyr::ungroup() |>
  dplyr::filter(game_type == 'POST') |>
  dplyr::select(season, week, club_code = away_team) |>
  dplyr::mutate(game_type = 'POST') |>
  dplyr::bind_rows(nflreadr::load_schedules() |>
                     dplyr::mutate(game_type = dplyr::case_when(game_type == 'REG' ~ 'REG',
                                                                game_type %in% c('WC','DIV','CON','SB') ~ 'POST',
                                                                T~NA_character_)) |>
                     dplyr::filter(!is.na(game_type)) |>
                     dplyr::group_by(season, game_type) |>
                     dplyr::mutate(week = as.numeric(as.factor(week))) |>
                     dplyr::ungroup() |>
                     dplyr::filter(game_type == 'POST') |>
                     dplyr::select(season, week, club_code = home_team) |>
                     dplyr::mutate(game_type = 'POST'))

scrape_teams <- function(year) {
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getClubs?lseason={year}"
    ),
    httr::authenticate("media", "media"),
    url = NULL
  )
  teams_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h)
  return(teams_df)
}

teams <- purrr::map_dfr(seasons_to_scrape, scrape_teams) |>
  dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) |> # remove all-star teams
  dplyr::mutate(Season = as.integer(Season)) |>
  dplyr::select(club_code = ClubCode, season = Season)

teams_reg <- teams |>
  tidyr::expand_grid(game_type = c("REG"),
                     week = c(1:18)) |>
  dplyr::inner_join(valid_weeks, by=c('season', 'game_type', 'week'))

teams_post <- teams |>
  tidyr::expand_grid(game_type = c('POST'),
                     week = c(1:4)) |>
  dplyr::inner_join(postseason_teams, by=c('season','game_type','week','club_code'))

teams <- teams_reg |>
  dplyr::bind_rows(teams_post)

scrape_rosters <- function(season, team, game_type, week) {
  cli::cli_process_start("Loading {season} {team}, week {week} of the {game_type} season")
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getRoster?lseason={season}&lweek={week}&lseasontype={game_type}&lclub={team}"
    ),
    httr::authenticate("media", "media"),
    url = NULL
  )
  roster_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h)
  cli::cli_process_done()
  return(roster_df)
}

roster_df <- purrr::pmap_dfr(list(teams$season, teams$club_code, teams$game_type, teams$week), scrape_rosters)

weeks_rostered_reg <- roster_df |>
  dplyr::filter(SeasonType == 'REG') |>
  dplyr::mutate(Season = as.double(Season),
                CurrentClub = dplyr::case_when(CurrentClub == 'ARZ' ~ 'ARI',
                                               CurrentClub == 'BLT' ~ 'BAL',
                                               CurrentClub == 'CLV' ~ 'CLE',
                                               CurrentClub == 'HST' ~ 'HOU',
                                               CurrentClub == 'SL' ~ 'STL',
                                               T~CurrentClub)) |>
  dplyr::group_by(gsis_id = GsisID, team = CurrentClub, season = Season) |>
  dplyr::summarize(weeks_active_reg = paste0(Week,collapse=';')) |>
  dplyr::ungroup()

weeks_rostered_post <- roster_df |>
  dplyr::filter(SeasonType == 'POST') |>
  dplyr::mutate(Season = as.double(Season),
                CurrentClub = dplyr::case_when(CurrentClub == 'ARZ' ~ 'ARI',
                                               CurrentClub == 'BLT' ~ 'BAL',
                                               CurrentClub == 'CLV' ~ 'CLE',
                                               CurrentClub == 'HST' ~ 'HOU',
                                               CurrentClub == 'SL' ~ 'STL',
                                               T~CurrentClub)) |>
  dplyr::group_by(gsis_id = GsisID, team = CurrentClub, season = Season) |>
  dplyr::summarize(weeks_active_post = paste0(Week,collapse=';')) |>
  dplyr::ungroup()

current_roster_df <- nflreadr::load_rosters(seasons = TRUE) |>
  dplyr::select(!dplyr::starts_with('weeks_active_')) |>
  dplyr::left_join(weeks_rostered_reg, by=c('gsis_id','team','season')) |>
  dplyr::left_join(weeks_rostered_post, by=c('gsis_id','team','season'))

roster_split <- current_roster_df |>
  dplyr::group_split(season)

purrr::walk(roster_split, function(x) {
  saveRDS(x, glue::glue("data/seasons/roster_{unique(x$season)}.rds"))
  readr::write_csv(x, glue::glue("data/seasons/roster_{unique(x$season)}.csv"))
})

saveRDS(current_roster_df, file = 'data/nflfastR-roster.rds')
readr::write_csv(current_roster_df, file = "data/nflfastR-roster.csv.gz")
