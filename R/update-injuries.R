message("Fetching schedule...")

weeks <- nflreadr::load_schedules() |>
  dplyr::filter(season >= 2009) |>
  dplyr::mutate(season_type = dplyr::case_when(
    game_type == "REG" ~ "REG",
    T ~ "POST"
  )) |>
  dplyr::rename(home_result = result) |>
  dplyr::group_by(season_type) |>
  dplyr::mutate(week = week - min(week) + 1) |>
  dplyr::filter(!is.na(home_result)) |>
  dplyr::group_by(season, week, season_type) |>
  dplyr::summarise() |>
  dplyr::arrange(season, season_type, week) |>
  dplyr::ungroup()

message("Scraping injury reports...")

scrape_ir <- function(year, week, season_type) {
  cli::cli_process_start("Loading {year}, {season_type} Week {week}")
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue("/nfldataexchange/dataexchange.asmx/getInjuryData?lseason={year}&lweek={week}&lseasontype={season_type}"),
    httr::authenticate("media", "media"),
    url = NULL
  )
  ir_df <- httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h) # close handle when finished, have had the api get mad when I don't close it
  cli::cli_process_done()
  return(ir_df)
}

ir_df <- purrr::pmap_dfr(list(weeks$season, weeks$week, weeks$season_type), scrape_ir) |>
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
    ModifiedDt = lubridate::as_datetime(ModifiedDt, format = "%s"),
    dplyr::across(
      c(Injury1:InjuryStatus, PracticeStatus:Practice2),
      ~ dplyr::case_when(
        .x == "--" ~ NA_character_,
        T ~ .x
      )
    )
  ) |>
  dplyr::select(
    season = Season,
    team = ClubCode,
    week = Week,
    gsis_id = GsisID,
    position = Position,
    full_name,
    first_name = FootballName,
    last_name = LastName,
    ir_primary_injury = Injury1,
    ir_secondary_injury = Injury2,
    ir_status = InjuryStatus,
    practice_primary_injury = Practice1,
    practice_secondary_injury = Practice2,
    practice_status = PracticeStatus,
    modified_dt = ModifiedDt
  ) |>
  tibble::as_tibble()

message("Save injury reports...")
saveRDS(ir_df, "data/nflfastR-injuries.rds")
readr::write_csv(ir_df, "data/nflfastR-injuries.csv.gz")
qs::qsave(ir_df, "data/nflfastR-injuries.qs",
          preset = "custom",
          algorithm = "zstd_stream",
          compress_level = 22,
          shuffle_control = 15)
rm(list = ls())
message("DONE!")

source("R/git.R")
message <- sprintf("Injury data updated %s (ET)", lubridate::now("America/New_York"))
git("commit", "-am", message)
