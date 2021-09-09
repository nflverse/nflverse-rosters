cli::cli_alert_info('Grabbing weekly availability...')
season_to_scrape <- nflreadr::load_schedules() |>
  dplyr::pull(season) |>
  max()

valid_weeks <- nflreadr::load_schedules(season_to_scrape) |>
  dplyr::mutate(game_type = dplyr::case_when(game_type == 'REG' ~ 'REG',
                                             game_type %in% c('WC','DIV','CON','SB') ~ 'POST',
                                             T~NA_character_)) |>
  dplyr::filter(!is.na(game_type)) |>
  dplyr::group_by(season, game_type) |>
  dplyr::mutate(week = as.numeric(as.factor(week))) |>
  dplyr::ungroup() |>
  dplyr::group_by(season, game_type, week) |>
  dplyr::summarise(games_played = sum(!is.na(result)), .groups = "drop_last") |>
  dplyr::ungroup() |>
  dplyr::mutate(filter_week = which(games_played == 0)[1]) |>
  dplyr::filter(dplyr::row_number() <= filter_week) |>
  dplyr::select(c(season, game_type = game_type, week))

postseason_teams <- nflreadr::load_schedules(season_to_scrape) |>
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

teams <- scrape_teams(season_to_scrape) |>
  dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) |>
  # remove all-star teams
  dplyr::mutate(Season = as.integer(Season)) |>
  dplyr::select(club_code = ClubCode, season = Season) |>
  tidyr::expand_grid(game_type = c("REG", "POST"),
                     week = c(1:18)) |>
  dplyr::inner_join(valid_weeks, by=c('season', 'game_type', 'week'))

scrape_rosters <- function(season, team, game_type, week) {
  # cli::cli_process_start("Loading {season} {team}, week {week} of the {game_type} season")
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
  # cli::cli_process_done()
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
  dplyr::summarize(weeks_active_reg = paste0(Week,collapse=';'), .groups = "drop_last") |>
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
  dplyr::summarize(weeks_active_post = paste0(Week,collapse=';'), .groups = "drop_last") |>
  dplyr::ungroup()

cli::cli_alert_info("Download raw JSON data...")
raw_json <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")

cli::cli_alert_info("Parse raw data...")
roster <-
  purrr::map_dfr(raw_json, function(x) purrr::map(x, function(y) ifelse(is.null(y), NA, y))) |>
  dplyr::na_if("") |>
  dplyr::mutate_if(is.character, stringr::str_trim) |>
  dplyr::filter(!(is.na(team) & is.na(gsis_id)), !player_id %in% nflfastR::teams_colors_logos$team_abbr, first_name != "Duplicate") |>
  dplyr::left_join(readRDS("R/na_map.rds"), by = c("sportradar_id" = "id")) |>
  dplyr::mutate(
    gsis_id = dplyr::if_else(is.na(gsis_id), gsis, gsis_id),
    update_dt = lubridate::now("America/New_York"),
    season = dplyr::if_else(
      lubridate::month(update_dt) < 3,
      lubridate::year(update_dt) - 1,
      lubridate::year(update_dt)
    ),
    index = 1:dplyr::n(),
    headshot_url = dplyr::if_else(is.na(espn_id), NA_character_, as.character(glue::glue("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png")))
  ) |>
  dplyr::mutate(
    # Tyler Conklin and Ryan Izzo could have swapped IDs and Christian Jones
    # falsely gets assigned Chris Jones gsis_id
    gsis_id = dplyr::case_when(
      full_name == "Tyler Conklin" & gsis_id != "00-0034270" ~ "00-0034270",
      full_name == "Ryan Izzo" & gsis_id != "00-0034439" ~ "00-0034439",
      full_name == "Christian Jones" & gsis_id != "00-0031130" ~ "00-0031130",
      TRUE ~ gsis_id
    ),
    espn_id = dplyr::case_when(
      full_name == "Tyler Conklin" & espn_id != 3915486L ~ 3915486L,
      full_name == "Ryan Izzo" & espn_id != 3122920L ~ 3122920L,
      TRUE ~ espn_id
    ),
    yahoo_id = dplyr::case_when(
      full_name == "Tyler Conklin" & yahoo_id != 31127L ~ 31127L,
      full_name == "Ryan Izzo" & yahoo_id != 31220L ~ 31220L,
      TRUE ~ yahoo_id
    )
  ) |>
  dplyr::left_join(readRDS("R/pff_gsis_map.rds"), by = "gsis_id")

dupl_ids <- roster |>
  dplyr::count(gsis_id) |>
  dplyr::filter(n > 1, !is.na(gsis_id)) |>
  dplyr::pull(gsis_id)

dupls_keep <- roster |>
  dplyr::filter(gsis_id %in% dupl_ids) |>
  dplyr::group_by(gsis_id) |>
  dplyr::arrange(desc(news_updated)) |>
  dplyr::filter(!is.na(sportradar_id)) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::pull(index)

dupls_remove <- roster |>
  dplyr::filter(gsis_id %in% dupl_ids, !index %in% dupls_keep) |>
  dplyr::pull(index)

roster <- roster |>
  dplyr::filter(!index %in% dupls_remove) |>
  dplyr::select(
    season,
    team,
    position,
    depth_chart_position,
    jersey_number = number,
    status,
    full_name,
    first_name,
    last_name,
    birth_date,
    height,
    weight,
    college,
    high_school,
    gsis_id,
    espn_id,
    sportradar_id,
    yahoo_id,
    rotowire_id,
    pff_id,
    fantasy_data_id,
    sleeper_id = player_id,
    # update_dt,
    years_exp,
    headshot_url
  ) |>
  dplyr::mutate(
    team = dplyr::case_when(
      team == "LAR" ~ "LA",
      team == "OAK" ~ "LV",
      TRUE ~ team
    ),
    height = stringr::str_remove_all(height, "\\\""),
    height = stringr::str_replace_all(height, "'", "-"),
    birth_date = lubridate::as_date(birth_date)
  ) |>
  dplyr::arrange(team, position) |>
  dplyr::left_join(weeks_rostered_reg, by=c('gsis_id', 'team', 'season')) |>
  dplyr::left_join(weeks_rostered_post, by=c('gsis_id', 'team', 'season'))

cli::cli_alert_info("Save stuff newest roster...")
saveRDS(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.rds"))
readr::write_csv(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.csv"))

cli::cli_alert_info("Build and safe combined roster file...")
latest_season <- unique(roster$season)
comb <- purrr::map_dfr(1999:latest_season, ~ readRDS(glue::glue("data/seasons/roster_{.x}.rds"))) |>
  dplyr::select(-tidyselect::any_of(c("update_dt")))
saveRDS(comb, "data/nflfastR-roster.rds")
readr::write_csv(comb, "data/nflfastR-roster.csv.gz")

# Safe RB & FB gsis IDs for usage in RACR computation
rb_ids <- comb |>
  dplyr::filter(position %in% c("RB", "FB"), !is.na(gsis_id)) |>
  dplyr::select(gsis_id) |>
  dplyr::distinct() |>
  dplyr::arrange(dplyr::desc(gsis_id))

qs::qsave(rb_ids, 'data/nflfastR-RB_ids.qs',
          preset = "custom",
          algorithm = "zstd_stream",
          compress_level = 22,
          shuffle_control = 15)

rm(list = ls())
cli::cli_alert_info("DONE!")
