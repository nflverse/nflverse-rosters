seasons_to_scrape <- nflreadr:::most_recent_season()

cli::cli_alert_info("Scraping teams...")

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
  rm(h) # close handle when finished, have had the api get mad when I don't close it
  return(teams_df)
}

teams <- purrr::map_dfr(seasons_to_scrape, scrape_teams) |>
  dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) |>
  # remove all-star teams
  dplyr::mutate(Season = as.integer(Season)) |>
  dplyr::select(club_code = ClubCode, season = Season) |>
  tidyr::expand_grid(season_type = c("REG", "POST"))

cli::cli_alert_info("Scraping depth charts...")

scrape_dc <- function(season, team, season_type) {
  cli::cli_process_start("Loading {season} {team}, {season_type}")
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getGameDepthChart?lSeason={season}&lSeasonType={season_type}&lWeek=0&lClub={team}"
    ),
    httr::authenticate("media", "media"),
    url = NULL
  )
  dc_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h)
  cli::cli_process_done()
  return(dc_df)
}

dc_df <-
  purrr::pmap_dfr(list(teams$season, teams$club_code, teams$season_type), scrape_dc) |>
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
    dplyr::across(c(Season, Week, DepthTeam, JerseyNumber), as.integer)
  ) |>
  dplyr::select(
    season = Season,
    week = Week,
    team = ClubCode,
    season_type = SeasonType,
    position = Position,
    depth_chart_position = DepthPosition,
    formation = Formation,
    depth_team = DepthTeam,
    jersey_number = JerseyNumber,
    full_name,
    first_name = FootballName,
    last_name = LastName,
    gsis_id = GsisID
  )

cli::cli_alert_info("Saving depth charts...")
dc_split <- dc_df |>
  dplyr::group_split(season)

purrr::walk(dc_split, function(x) {
  saveRDS(x, glue::glue("data/seasons/depth_charts_{unique(x$season)}.rds"))
  readr::write_csv(x, glue::glue("data/seasons/depth_charts_{unique(x$season)}.csv.gz"))
})

full_dc_df <- list.files("data/seasons", pattern = "depth_charts_[0-9]+\\.rds", full.names = TRUE) |>
  purrr::map_dfr(readRDS)

saveRDS(full_dc_df, "data/nflfastR-depth_charts.rds")
readr::write_csv(full_dc_df, "data/nflfastR-depth_charts.csv.gz")
qs::qsave(
  full_dc_df,
  "data/nflfastR-depth_charts.qs",
  preset = "custom",
  algorithm = "zstd_stream",
  compress_level = 22,
  shuffle_control = 15
)

cli::cli_alert_success("Finished scraping depth charts!")
