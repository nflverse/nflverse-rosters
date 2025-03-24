scrape_teams <- function(season) {
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getClubs?lseason={season}"
    ),
    httr::authenticate(
      Sys.getenv("NFLDX_USERNAME", "media"),
      Sys.getenv("NFLDX_PASSWORD", "media")
    ),
    url = NULL
  )
  teams_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h) # close handle when finished, have had the api get mad when I don't close it
  return(teams_df)
}

scrape_dc <- function(season, team, season_type) {
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getGameDepthChart?lSeason={season}&lSeasonType={season_type}&lWeek=0&lClub={team}"
    ),
    httr::authenticate(
      Sys.getenv("NFLDX_USERNAME", "media"),
      Sys.getenv("NFLDX_PASSWORD", "media")
    ),
    url = NULL
  )
  dc_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h)
  return(dc_df)
}

build_dc <-
  function(season = nflreadr:::most_recent_season(roster = T)) {
    cli::cli_alert_info("Scraping teams for {season}...")

    teams <- scrape_teams(season) |>
      dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) |>
      # remove all-star teams
      dplyr::mutate(Season = as.integer(Season)) |>
      dplyr::select(season = Season, team = ClubCode) |>
      tidyr::expand_grid(season_type = c("REG", "POST"))

    cli::cli_alert_info("Scraping depth charts for {season}...")

    dc_df <-
      purrr::pmap(teams, scrape_dc, .progress = T) |>
      purrr::list_rbind()

    if (nrow(dc_df)) {
      dc_df <- dc_df |>
        dplyr::mutate(
          ClubCode = dplyr::case_when(
            ClubCode == "ARZ" ~ "ARI",
            ClubCode == "BLT" ~ "BAL",
            ClubCode == "CLV" ~ "CLE",
            ClubCode == "HST" ~ "HOU",
            ClubCode == "SL" ~ "STL",
            T ~ ClubCode
          ),
          full_name = paste(FootballName, LastName),
          Season = as.numeric(Season),
          Week = as.numeric(Week),
        ) |>
        janitor::clean_names() |>
        dplyr::rename(game_type = season_type) |>
        dplyr::group_by(season) |>
        dplyr::mutate(
          game_type = dplyr::case_when(
            game_type == "POST" & week == 1 ~ "WC",
            game_type == "POST" &
              week == 2 ~ "DIV",
            game_type == "POST" &
              week == 3 ~ "CON",
            game_type == "POST" &
              ((
                week == 4 & season %in% c(2002:2006, 2014)
              ) |
                (week == 5)) ~ "SB",
            game_type == "POST" &
              (week == 4 &
                 !(season %in% c(2002:2006, 2014))) ~ "SBBYE", # sometimes we get bye week depth charts for the SB
            T ~ game_type
          ),
          week = dplyr::case_when(
            game_type == "SBBYE" ~ NA_integer_, # the bye week isn't counted in `load_schedules()` so we treat it like it's not an official NFL week
            game_type == "SB" & season == 2001 ~ week + max(week[game_type == "REG"]) - 1,
            game_type == "SB" ~ week + max(week[game_type == "REG"]) - dplyr::if_else(season >= 2007 & season != 2014, 2, 0),
            game_type %in% c("WC", "DIV", "CON") ~ week + max(week[game_type == "REG"]) - dplyr::if_else(season >= 2007 & season != 2014, 1, 0),
            T ~ week,
          )
        ) |>
        dplyr::ungroup()

      cli::cli_alert_info("Save depth charts for {season}...")
      nflversedata::nflverse_save(
        data_frame = dc_df,
        file_name =  glue::glue("depth_charts_{season}"),
        nflverse_type = "depth charts",
        release_tag = "depth_charts"
      )
    } else {
      cli::cli_alert_warning("No depth charts found for {season}!")
    }
    cli::cli_alert_success("Job's done.")
  }

# purrr::walk(2001:2023, build_dc)

build_dc()
