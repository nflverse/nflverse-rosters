scrape_teams <- function(season) {
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getClubs?lseason={season}"
    ),
    httr::authenticate(Sys.getenv("NFLDX_USERNAME", "media"), Sys.getenv("NFLDX_PASSWORD", "media")),
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
    httr::authenticate(Sys.getenv("NFLDX_USERNAME", "media"), Sys.getenv("NFLDX_PASSWORD", "media")),
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
    cli::cli_alert_info("Scraping teams...")

    teams <- purrr::map_dfr(season, scrape_teams) |>
      dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) |>
      # remove all-star teams
      dplyr::mutate(Season = as.integer(Season)) |>
      dplyr::select(club_code = ClubCode, season = Season) |>
      tidyr::expand_grid(season_type = c("REG", "POST"))

    cli::cli_alert_info("Scraping depth charts...")

    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(teams))
      dc_df <-
        purrr::pmap_dfr(list(teams$season, teams$club_code, teams$season_type),
                        \(x, y, z) {
                          df <- scrape_dc(x, y, z)
                          p()
                          return(df)
                        })
    })

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
              week == 4 ~ "SB",
            T ~ game_type
          ),
          week = dplyr::case_when(
            game_type %in% c("WC", "DIV", "CON", "SB") ~ week + max(week[game_type == "REG"]),
            T ~ week
          ),

        ) |>
        dplyr::ungroup()

      cli::cli_alert_info("Save depth charts...")
      nflversedata::nflverse_save(
        data_frame = dc_df,
        file_name =  glue::glue("depth_charts_{season}"),
        nflverse_type = "depth charts",
        release_tag = "depth_charts"
      )

    }
  }


# purrr::walk(2001:2022, build_dc)

build_dc()
