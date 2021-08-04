most_rec_season = stringi::stri_extract_all_regex(dir('data/seasons'), 'injuries_[0-9]{4}') |>
  unlist() |>
  na.omit() |>
  max() |>
  (\(x)gsub('injuries_', '', x))()
most_rec_season = ifelse(is.na(most_rec_season),2009,most_rec_season)

cli::cli_alert_info("Fetching schedule...")

weeks <- nflreadr::load_schedules() |>
  dplyr::filter(season >= most_rec_season) |>
  dplyr::mutate(season_type = dplyr::case_when(game_type == "REG" ~ "REG",
                                               T ~ "POST")) |>
  dplyr::rename(home_result = result) |>
  dplyr::group_by(season_type) |>
  dplyr::mutate(week = week - min(week) + 1) |>
  dplyr::filter(!is.na(home_result)) |>
  dplyr::ungroup() |>
  dplyr::distinct(season,week,season_type)

cli::cli_alert_info("Scraping injury reports...")

scrape_ir <- function(year, week, season_type) {
  cli::cli_process_start("Loading {year}, {season_type} Week {week}", on_exit = "done")

  h <- httr::handle("https://www.nfl.info")
  on.exit(rm(h), add = TRUE) # close handle when function exits

  r <- try({
    httr::GET(
      handle = h,
      path = glue::glue(
        "/nfldataexchange/dataexchange.asmx/getInjuryData?lseason={year}&lweek={week}&lseasontype={season_type}"
      ),
      httr::authenticate("media", "media"),
      url = NULL) |>
      httr::content() |>
      XML::xmlParse() |>
      XML::xmlToDataFrame()
  }, silent = TRUE)

  if(inherits(r,"try-error")) {
    cli::cli_process_failed()
    return(data.frame())
  }

  return(r)
}

ir_df <-
  purrr::pmap_dfr(list(weeks$season, weeks$week, weeks$season_type), scrape_ir) |>
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
    date_modified = lubridate::as_datetime(ModifiedDt, format = "%s"),
    dplyr::across(
      c(Injury1:InjuryStatus, PracticeStatus:Practice2),
      ~ dplyr::case_when(.x == "--" ~ NA_character_,
                         T ~ .x)
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
    report_primary_injury = Injury1,
    report_secondary_injury = Injury2,
    report_status = InjuryStatus,
    practice_primary_injury = Practice1,
    practice_secondary_injury = Practice2,
    practice_status = PracticeStatus,
    date_modified
  )

cli::cli_alert_info("Saving injury reports...")

ir_split <- ir_df |>
  dplyr::group_split(season)

purrr::walk(ir_split, function(x) {
  saveRDS(x, glue::glue('data/seasons/injuries_{unique(x$season)}.rds'))
  readr::write_csv(x,
                   glue::glue('data/seasons/injuries_{unique(x$season)}.csv.gz'))
  qs::qsave(
    x,
    glue::glue('data/seasons/injuries_{unique(x$season)}.qs'),
    preset = "custom",
    algorithm = "zstd_stream",
    compress_level = 22,
    shuffle_control = 15
  )
})

full_ir_df <- list.files("data/seasons",pattern = "injuries_[0-9]+\\.qs",full.names = TRUE) |>
  purrr::map_dfr(qs::qread)

saveRDS(full_ir_df, "data/nflfastR-injuries.rds")
readr::write_csv(full_ir_df, "data/nflfastR-injuries.csv.gz")
qs::qsave(
  full_ir_df,
  "data/nflfastR-injuries.qs",
  preset = "custom",
  algorithm = "zstd_stream",
  compress_level = 22,
  shuffle_control = 15
)

cli::cli_alert_success("Finished scraping injuries!")
