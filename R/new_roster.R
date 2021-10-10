"%>%" <- magrittr::`%>%`

raw_json <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")

roster <-
  purrr::map_dfr(raw_json, function(x) purrr::map(x, function(y) ifelse(is.null(y), NA, y))) %>%
  dplyr::na_if("") %>%
  dplyr::filter(!(is.na(team) & is.na(gsis_id)),
                !player_id %in% nflreadr::load_teams()$team_abbr,
                first_name != "Duplicate") %>%
  dplyr::filter(!player_id %in% nflreadr::load_teams()$team_abbr,
                first_name != "Duplicate") %>%
  dplyr::mutate(
    update_dt = lubridate::now("America/New_York"),
    season = dplyr::if_else(
      lubridate::month(update_dt) < 3,
      lubridate::year(update_dt) - 1,
      lubridate::year(update_dt)
    ),
    headshot_url = dplyr::if_else(is.na(espn_id), NA_character_, as.character(glue::glue("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png")))
  ) %>%
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
    update_dt,
    headshot_url
  ) %>%
  dplyr::arrange(team, position)

old <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>%
  dplyr::mutate(
    depth_chart_position = NA,
    high_school = NA,
    espn_id = NA,
    sportradar_id = NA,
    yahoo_id = NA,
    rotowire_id = NA,
    update_dt = NA,
    teamPlayers.weight = as.character(teamPlayers.weight)
  ) %>%
  dplyr::select(
    season = team.season,
    team = team.abbr,
    position = teamPlayers.position,
    depth_chart_position,
    jersey_number = teamPlayers.jerseyNumber,
    status = teamPlayers.status,
    full_name = teamPlayers.displayName,
    first_name = teamPlayers.firstName,
    last_name = teamPlayers.lastName,
    birth_date = teamPlayers.birthDate,
    height = teamPlayers.height,
    weight = teamPlayers.weight,
    college = teamPlayers.collegeName,
    high_school,
    gsis_id = teamPlayers.gsisId,
    espn_id,
    sportradar_id,
    yahoo_id,
    rotowire_id,
    update_dt,
    headshot_url = teamPlayers.headshot_url
  )

join <- dplyr::bind_rows(old, roster) %>%
  dplyr::group_by(gsis_id) %>%
  dplyr::mutate(
    high_school = dplyr::last(stats::na.omit(high_school)),
    espn_id = dplyr::last(stats::na.omit(espn_id)),
    sportradar_id = dplyr::last(stats::na.omit(sportradar_id)),
    yahoo_id = dplyr::last(stats::na.omit(yahoo_id)),
    rotowire_id = dplyr::last(stats::na.omit(rotowire_id)),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    headshot_url = dplyr::if_else(
      is.na(headshot_url) & !is.na(espn_id),
      glue::glue("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png"),
      headshot_url
    )
  ) %>%
  dplyr::arrange(season, team, position)

purrr::walk(unique(join$season), function(x, df){
  filt <- df %>% dplyr::filter(season == x)
  saveRDS(filt, glue::glue("seasons/roster_{x}.rds"))
  readr::write_csv(filt, glue::glue("seasons/roster_{x}.csv"))
}, join)

# fix birth_date for older seasons
purrr::walk(1999:2019, function(x){
  r <- readRDS(glue::glue("data/seasons/roster_{x}.rds")) %>%
    dplyr::mutate(birth_date = lubridate::as_date(birth_date, format = "%m/%d/%Y"))
  saveRDS(r, glue::glue("data/seasons/roster_{x}.rds"))
  readr::write_csv(r, glue::glue("data/seasons/roster_{x}.csv"))
})

# join pff ids
purrr::walk(1999:2019, function(x){
  r <- readRDS(glue::glue("data/seasons/roster_{x}.rds")) %>%
    dplyr::left_join(
      readRDS(glue::glue("R/pff_gsis_map.rds")),
      by = "gsis_id"
    ) %>%
    dplyr::select(season:rotowire_id, pff_id, dplyr::everything())
  saveRDS(r, glue::glue("data/seasons/roster_{x}.rds"))
  readr::write_csv(r, glue::glue("data/seasons/roster_{x}.csv"))
})
