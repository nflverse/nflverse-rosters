"%>%" <- magrittr::`%>%`
message("Download raw JSON data...")
raw_json <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")

message("Parse raw data...")
roster <-
  purrr::map_dfr(raw_json, function(x) purrr::map(x, function(y) ifelse(is.null(y), NA, y))) %>%
  dplyr::na_if("") %>%
  dplyr::filter(!(is.na(team) & is.na(gsis_id)), !player_id %in% nflfastR::teams_colors_logos$team_abbr, first_name != "Duplicate") %>%
  dplyr::filter(!player_id %in% nflfastR::teams_colors_logos$team_abbr, first_name != "Duplicate") %>%
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

message("Save stuff...")
saveRDS(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.rds"))
readr::write_csv(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.csv"))

rm(list = ls())

message("DONE!")
