#' Build weekly rosters from data exchange
#' @param season integer: season to build
#'
#' @return dataframe of weekly rosters
build_rosters_weekly_dataexchange <- function(season,
                                              username = Sys.getenv("NFLDX_USERNAME", "media"),
                                              password = Sys.getenv("NFLDX_PASSWORD", "media")) {
  cli::cli_alert_info("Using the NFL Data Exchange to obtain roster information!")
  cli::cli_alert_info("Scraping team and weeks for {season}...")
  r <- httr::GET(
    handle = httr::handle("https://www.nfl.info"),
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getClubs?lseason={season}"
    ),
    httr::authenticate(username, password),
    url = NULL
  )
  teams <- httr::content(r) |>
    xml2::as_list() |>
    tibble::as_tibble() |>
    tidyr::unnest_wider(Clubs) |>
    tidyr::unnest(tidyselect::everything()) |>
    tidyr::unnest(tidyselect::everything()) |>
    dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV")))

  weeks <-
    purrr::map_dfr(c("REG", "POST"), ~ ngsscrapR::scrape_schedule(season, .x)) |>
    dplyr::group_by(week, game_type) |>
    dplyr::summarise(.groups = "drop")

  team_gmtype_pair <- teams |>
    dplyr::select(teamId = ClubCode, Season) |>
    dplyr::mutate(join = 1) |>
    dplyr::left_join(data.frame(seasonType = c("REG", "POST"), join = c(1, 1)), by = c("join")) |>
    dplyr::select(-c(join))

  cli::cli_alert_info("Scraping rosters for {season}...")
  scrape_rosters <- function() {
    p <- progressr::progressor(along = 1:nrow(team_gmtype_pair))
    weekly_rosters <-
      purrr::pmap_dfr(
        list(
          team_gmtype_pair$Season,
          team_gmtype_pair$seasonType,
          team_gmtype_pair$teamId
        ),
        \(x, y, z) {
          r <- httr::GET(
            handle = httr::handle("https://www.nfl.info"),
            path = glue::glue(
              "/nfldataexchange/dataexchange.asmx/getRoster?lSeason={x}&lSeasonType={y}&lWeek=0&lClub={z}"
            ),
            httr::authenticate(username, password),
            url = NULL
          )

          if (httr::content(r) |>
              xml2::xml_contents() |>
              length() != 0) {
            roster <- httr::content(r) |>
              xml2::as_list() |>
              tibble::as_tibble() |>
              tidyr::unnest_wider(Roster) |>
              tidyr::unnest(tidyselect::everything()) |>
              tidyr::unnest(tidyselect::everything()) |>
              data.frame()
          } else {
            roster <- data.frame()
          }
          p()
          return(roster)
        }
      )
  }

  progressr::with_progress({
    weekly_rosters <- scrape_rosters() |>
      dplyr::mutate(
        depth_chart_position = NA_character_,
        years_exp = season - as.integer(EntryYear),
        gsis_it_id = NA_integer_,
        display_name = FootballName,
        short_name = NA_character_,
        uniform_number = JerseyNumber,
        status = NA_character_,
        current_team_id = NA_character_,
        smart_id = NA_integer_,
        headshot = NA_character_,
        position_group = NA_character_,
        ngs_position = NA_character_,
        ngs_position_group = NA_character_
      ) |>
      dplyr::select(
        Season,
        game_type = SeasonType,
        Week,
        JerseyNumber,
        LastName,
        FootballName,
        FirstName,
        Position,
        EntryYear,
        RookieYear,
        Height,
        Weight,
        DraftClub,
        DraftNumber,
        CollegeConference,
        StatusDescriptionAbbr,
        StatusShortDescription,
        gsis_it_id,
        GsisID,
        esb_id = EliasID,
        display_name,
        short_name,
        uniform_number,
        status,
        team_abbr = CurrentClub,
        current_team_id,
        smart_id,
        headshot,
        position_group,
        ngs_position,
        ngs_position_group,
        depth_chart_position,
        years_exp
      ) |>
      tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
      dplyr::group_by(game_type) |>
      dplyr::mutate(week = dplyr::dense_rank(as.numeric(week))) |>
      dplyr::ungroup() |>
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
        display_name = paste(display_name, last_name)
      )
  })
  return(weekly_rosters)
}
