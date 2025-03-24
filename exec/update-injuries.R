scrape_ir <- function(year, week, game_type) {
  h <- httr::handle("https://www.nfl.info")
  on.exit(rm(h), add = TRUE) # close handle when function exits

  r <- try({
    httr::GET(
      handle = h,
      path = glue::glue(
        "/nfldataexchange/dataexchange.asmx/getInjuryData?lseason={year}&lweek={week}&lseasontype={game_type}"
      ),
      httr::authenticate(Sys.getenv("NFLDX_USERNAME", "media"), Sys.getenv("NFLDX_PASSWORD", "media")),
      url = NULL
    ) |>
      httr::content() |>
      XML::xmlParse() |>
      XML::xmlToDataFrame()
  }, silent = TRUE)

  if (inherits(r, "try-error")) {
    return(data.frame())
  }

  return(r)
}

build_ir <- function(season) {
  cli::cli_alert_info("Building injury reports...")

  weeks <- expand.grid(
    season = c(season),
    week = c(1:18),
    game_type = c("REG", "POST")
  ) |>
    dplyr::filter(
      (season <= 2020 & week <= 17 & game_type == "REG") |
        (season > 2020 & game_type == "REG") |
        (game_type == "POST" & week <= 5)
    )

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(weeks))

    ir_df <-
      purrr::pmap_dfr(list(weeks$season, weeks$week, weeks$game_type), \(x, y, z) {
        df <- scrape_ir(x, y, z)
        p()
        return(df)
      }) |>
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
        ),
        Season = as.numeric(Season),
        Week = as.numeric(Week)
      ) |>
      dplyr::select(
        season = Season,
        game_type = SeasonType,
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
      ) |>
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
  })

  cli::cli_alert_info("Saving injury reports...")

  ir_split <- ir_df |>
    dplyr::group_split(season)

  purrr::walk(ir_split, function(x) {
    nflversedata::nflverse_save(
      data_frame = x,
      file_name = paste0("injuries_", unique(x$season)),
      nflverse_type = "injury & practice reports",
      release_tag = "injuries"
    )
  })
}

# build all seasons
# build_ir(2009:nflreadr:::most_recent_season())

# update most recent season
build_ir(nflreadr:::most_recent_season())
