build_rosters <- function(season = nflreadr::most_recent_season(roster = TRUE)){
  teams <- ngsscrapR::scrape_teams(season) |>
    dplyr::filter(!is.na(division_id))
  # weeks <- purrr::map_dfr(c("REG","POST"), ~ngsscrapR::scrape_schedule(season, .x)) |>
  #   dplyr::group_by(week, game_type) |>
  #   dplyr::summarise()
  # team_week_pair <- teams |>
  #   dplyr::select(team_id) |>
  #   dplyr::mutate(join = 1) |>
  #   dplyr::left_join(weeks |>
  #                      dplyr::mutate(join = 1),by=c("join")) |>
  #   dplyr::select(-c(join)) |>
  #   dplyr::rename(teamId = team_id,
  #                 seasonType = game_type) |>
  #   dplyr::mutate(season = !!season)

  cli::cli_alert_info("Obtaining roster data...")
  rosters <- purrr::map_dfr(teams$team_id, ngsscrapR::scrape_current_roster, season)

  # cli::cli_alert_info("Obtaining player information...")
  # players <- purrr::map_dfr(rosters$gsis_id, ngsscrapR::scrape_player)

  cli::cli_alert_info("Download raw JSON data...")
  raw_json <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")
  sleeper <- purrr::map_dfr(raw_json, function(x) purrr::map(x, function(y) ifelse(is.null(y), NA, y))) |>
    dplyr::na_if("") |>
    dplyr::mutate_if(is.character, stringr::str_trim) |>
    dplyr::filter(!(is.na(team) & is.na(gsis_id)),
                  !player_id %in% nflreadr::load_teams()$team_abbr,
                  first_name != "Duplicate") |>
    dplyr::left_join(readRDS("src/na_map.rds"), by = c("sportradar_id" = "id")) |>
    dplyr::mutate(
      gsis_id = dplyr::if_else(is.na(gsis_id), gsis, gsis_id),
      update_dt = lubridate::now("America/New_York"),
      season = dplyr::if_else(
        lubridate::month(update_dt) < 3,
        lubridate::year(update_dt) - 1,
        lubridate::year(update_dt)
      ),
      index = 1:dplyr::n()
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
    dplyr::left_join(readRDS("src/headshot_gsis_map.rds"), by = "gsis_id") |>
    dplyr::mutate(
      headshot_url = dplyr::case_when(
        !is.na(headshot_nfl) ~ headshot_nfl,
        !is.na(espn_id) ~ as.character(glue::glue("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png")),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::left_join(readRDS("src/pff_gsis_map.rds"), by = "gsis_id") |>
    dplyr::left_join(readRDS("src/pfr_gsis_map.rds"), by = "gsis_id")

  rosters |>
    dplyr::left_join(sleeper |>
                dplyr::select(gsis_id, depth_chart_position, birth_date, college, high_school,
                              espn_id,
                              sportradar_id,
                              yahoo_id,
                              rotowire_id,
                              pff_id,
                              pfr_id,
                              fantasy_data_id,
                              sleeper_id = player_id), by=c("gsis_id")) |>
    dplyr::mutate(years_exp = nflreadr:::most_recent_season() - entry_year,
           headshot_url = gsub("\\{formatInstructions\\}","f_auto,q_auto",headshot)) |>
    dplyr::select(
      season,
      team = team_abbr,
      position,
      depth_chart_position,
      jersey_number,
      status = status_short_description,
      full_name = display_name,
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
      pfr_id,
      fantasy_data_id,
      sleeper_id,
      years_exp,
      headshot_url,
      ngs_position
    )
