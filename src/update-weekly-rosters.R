build_rosters <-
  function(season = nflreadr:::most_recent_season(roster = TRUE)) {
    if (season < 2002) {
      cli::cli_alert_info("Using Sleeper to obtain roster information!")
      raw_json <-
        jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")

      roster <-
        purrr::map_dfr(raw_json, function(x)
          purrr::map(x, function(y)
            ifelse(is.null(y), NA, y))) |>
        dplyr::na_if("") |>
        dplyr::mutate_if(is.character, stringr::str_trim) |>
        dplyr::filter(
          !(is.na(team) & is.na(gsis_id)),!player_id %in% nflreadr::load_teams()$team_abbr,
          first_name != "Duplicate"
        ) |>
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
            full_name == "Tyler Conklin" &
              gsis_id != "00-0034270" ~ "00-0034270",
            full_name == "Ryan Izzo" &
              gsis_id != "00-0034439" ~ "00-0034439",
            full_name == "Christian Jones" &
              gsis_id != "00-0031130" ~ "00-0031130",
            TRUE ~ gsis_id
          ),
          espn_id = dplyr::case_when(
            full_name == "Tyler Conklin" & espn_id != 3915486L ~ 3915486L,
            full_name == "Ryan Izzo" &
              espn_id != 3122920L ~ 3122920L,
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
            !is.na(headshot_nfl) ~ headshot_nfl,!is.na(espn_id) ~ as.character(
              glue::glue(
                "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png"
              )
            ),
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(readRDS("src/pff_gsis_map.rds"), by = "gsis_id") |>
        dplyr::left_join(readRDS("src/pfr_gsis_map.rds"), by = "gsis_id")

      dupl_ids <- roster |>
        dplyr::count(gsis_id) |>
        dplyr::filter(n > 1,!is.na(gsis_id)) |>
        dplyr::pull(gsis_id)

      dupls_keep <- roster |>
        dplyr::filter(gsis_id %in% dupl_ids) |>
        dplyr::group_by(gsis_id) |>
        dplyr::arrange(desc(news_updated)) |>
        dplyr::filter(!is.na(sportradar_id)) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::pull(index)

      dupls_remove <- roster |>
        dplyr::filter(gsis_id %in% dupl_ids,!index %in% dupls_keep) |>
        dplyr::pull(index)

      roster <- roster |>
        dplyr::filter(!index %in% dupls_remove) |>
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
          pff_id,
          pfr_id,
          fantasy_data_id,
          sleeper_id = player_id,
          # update_dt,
          years_exp,
          headshot_url
        ) |>
        dplyr::mutate(
          team = dplyr::case_when(team == "LAR" ~ "LA",
                                  team == "OAK" ~ "LV",
                                  TRUE ~ team),
          height = stringr::str_remove_all(height, "\\\""),
          height = stringr::str_replace_all(height, "'", "-"),
          birth_date = lubridate::as_date(birth_date),
          ngs_position = NA_character_,
          esb_id = NA_character_,
          gsis_it_id = NA_character_,
          smart_id = NA_character_,
          entry_year = NA_integer_,
          rookie_year = NA_integer_
        ) |>
        dplyr::arrange(team, position)
    } else {
      if (season >= 2016) {
        cli::cli_alert_info("Using NGS API to obtain roster information!")
        cli::cli_alert_info("Scraping team and weeks for {season}...")
        teams <- ngsscrapR::scrape_teams(season) |>
          dplyr::filter(!is.na(division_id))
        weeks <-
          purrr::map_dfr(c("REG", "POST"),
                         ~ ngsscrapR::scrape_schedule(season, .x)) |>
          dplyr::group_by(week, game_type) |>
          dplyr::summarise(.groups = "drop")
        team_week_pair <- teams |>
          dplyr::select(team_id) |>
          dplyr::mutate(join = 1) |>
          dplyr::left_join(weeks |>
                             dplyr::mutate(join = 1), by = c("join")) |>
          dplyr::select(-c(join)) |>
          dplyr::rename(teamId = team_id,
                        seasonType = game_type) |>
          dplyr::mutate(
            season = !!season,
            seasonType = dplyr::case_when(seasonType != "REG" ~ "POST",
                                          T ~ seasonType)
          )
        cli::cli_alert_info("Scraping rosters for {season}...")
        scrape_rosters <- function() {
          p <- progressr::progressor(along = 1:nrow(team_week_pair))
          weekly_rosters <-
            purrr::pmap_dfr(
              list(
                team_week_pair$teamId,
                team_week_pair$season,
                team_week_pair$seasonType,
                team_week_pair$week
              ),
              \(w, x, y, z) {
                roster <- ngsscrapR::scrape_roster(w, x, y, z)
                p()
                return(roster)
              }
            )
        }
        progressr::with_progress({
          weekly_rosters <- scrape_rosters()
        })
      } else if (season >= 2002) {
        cli::cli_alert_info("Using the NFL Data Exchange to obtain roster information!")
        cli::cli_alert_info("Scraping team and weeks for {season}...")
        r <- httr::GET(
          handle = httr::handle('https://www.nfl.info') ,
          path = glue::glue(
            '/nfldataexchange/dataexchange.asmx/getClubs?lseason={season}'
          ),
          httr::authenticate('media', 'media'),
          url = NULL
        )
        teams <- httr::content(r) |>
          xml2::xml_contents() |>
          xml2::as_list() |>
          unlist() |>
          stack() |>
          unstack() |>
          dplyr::filter(!(ClubCode %in% c(
            'AFC', 'NFC', 'RIC', 'SAN', 'CRT', 'IRV'
          )))

        weeks <-
          purrr::map_dfr(c("REG", "POST"),
                         ~ ngsscrapR::scrape_schedule(season, .x)) |>
          dplyr::group_by(week, game_type) |>
          dplyr::summarise(.groups = "drop")

        team_gmtype_pair <- teams |>
          dplyr::select(teamId = ClubCode, Season) |>
          dplyr::mutate(join = 1) |>
          dplyr::left_join(data.frame(
            seasonType = c("REG", "POST"),
            join = c(1, 1)
          ), by = c("join")) |>
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
                  handle = httr::handle('https://www.nfl.info') ,
                  path = glue::glue(
                    '/nfldataexchange/dataexchange.asmx/getRoster?lSeason={x}&lSeasonType={y}&lWeek=0&lClub={z}'
                  ),
                  httr::authenticate('media', 'media'),
                  url = NULL
                )

                if (httr::content(r) |>
                    xml2::xml_contents() |>
                    length() != 0) {
                  roster <- httr::content(r) |>
                    xml2::xml_contents() |>
                    xml2::as_list() |>
                    unlist() |>
                    stack() |>
                    unstack(values ~ ind) |>
                    (\(x) lapply(x, `length<-`, max(lengths(x))))() |>
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
              SeasonType,
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
              ngs_position_group
            ) |>
            tibble::as_tibble(.name_repair = janitor::make_clean_names)
        })
      } else {
        cli::cli_alert_info("Using the Sleeper API to obtain roster information!")
        raw_json <-
          jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")
      }

      roster <- weekly_rosters |>
        dplyr::group_by(season, team_abbr, week) |>
        dplyr::mutate(group_id = dplyr::cur_group_id()) |>
        dplyr::ungroup() |>
        dplyr::group_by(season, team_abbr) |>
        dplyr::filter(group_id == max(group_id)) |>
        dplyr::ungroup()

      players <-
        piggyback::pb_download_url(file = "players.rds",
                                   repo = "nflverse/nflverse-data",
                                   tag = "players") |>
        nflreadr::rds_from_url()

      cli::cli_alert_info("Download raw JSON data...")
      raw_json <-
        jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl")
      sleeper <-
        purrr::map_dfr(raw_json, function(x)
          purrr::map(x, function(y)
            ifelse(is.null(y), NA, y))) |>
        dplyr::na_if("") |>
        dplyr::mutate_if(is.character, stringr::str_trim) |>
        dplyr::filter(
          !(is.na(team) &
              is.na(gsis_id)),!player_id %in% nflreadr::load_teams()$team_abbr,
          first_name != "Duplicate"
        ) |>
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
            full_name == "Tyler Conklin" &
              gsis_id != "00-0034270" ~ "00-0034270",
            full_name == "Ryan Izzo" &
              gsis_id != "00-0034439" ~ "00-0034439",
            full_name == "Christian Jones" &
              gsis_id != "00-0031130" ~ "00-0031130",
            TRUE ~ gsis_id
          ),
          espn_id = dplyr::case_when(
            full_name == "Tyler Conklin" & espn_id != 3915486L ~ 3915486L,
            full_name == "Ryan Izzo" &
              espn_id != 3122920L ~ 3122920L,
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
            !is.na(headshot_nfl) ~ headshot_nfl,!is.na(espn_id) ~ as.character(
              glue::glue(
                "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png"
              )
            ),
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(readRDS("src/pff_gsis_map.rds"), by = "gsis_id") |>
        dplyr::left_join(readRDS("src/pfr_gsis_map.rds"), by = "gsis_id")

      roster <- roster |>
        dplyr::select(-dplyr::any_of("birth_date")) |>
        dplyr::left_join(
          players |>
            dplyr::select(gsis_id, college = college_name, birth_date),
          by = c("gsis_id")
        ) |>
        dplyr::left_join(
          sleeper |>
            dplyr::select(
              gsis_id,
              depth_chart_position,
              high_school,
              espn_id,
              sportradar_id,
              yahoo_id,
              rotowire_id,
              pff_id,
              pfr_id,
              fantasy_data_id,
              sleeper_id = player_id
            ),
          by = c("gsis_id")
        ) |>
        dplyr::mutate(
          years_exp = nflreadr:::most_recent_season() - as.integer(entry_year),
          headshot_url = gsub("\\{formatInstructions\\}", "f_auto,q_auto", headshot)
        ) |>
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
          dplyr::any_of(
            c(
              "ngs_position",
              "esb_id",
              "gsis_it_id",
              "smart_id",
              "entry_year",
              "rookie_year"
            )
          )
        ) |>
        dplyr::mutate(
          team = dplyr::case_when(team == "LAR" ~ "LA",
                                  team == "OAK" ~ "LV",
                                  TRUE ~ team),
          height = stringr::str_remove_all(height, "\\\""),
          height = stringr::str_replace_all(height, "'", "-"),
          birth_date = lubridate::as_date(birth_date)
        ) |>
        dplyr::arrange(team, position)
    }

    cli::cli_alert_info("Save stuff newest roster...")
    nflversedata::nflverse_save(
      data_frame = roster,
      file_name =  glue::glue("roster_{season}"),
      nflverse_type = "roster data",
      release_tag = "rosters")
    # saveRDS(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.rds"))
    # readr::write_csv(roster, glue::glue("data/seasons/roster_{unique(roster$season)}.csv"))

    nflversedata::nflverse_save(
      data_frame = weekly_rosters,
      file_name =  glue::glue("roster_weekly_{season}"),
      nflverse_type = "weekly roster data",
      release_tag = "weekly rosters")

    cli::cli_alert_info("Build and save combined roster file...")
    latest_season <- unique(roster$season)
    comb <- purrr::map_dfr(1999:latest_season, ~ readRDS(glue::glue("data/seasons/roster_{.x}.rds"))) |>
      dplyr::select(-tidyselect::any_of(c("update_dt"))) |>
      dplyr::left_join(readRDS("src/pfr_gsis_map.rds"), by = "gsis_id", suffix = c("", "_joined")) |>
      dplyr::mutate(pfr_id = dplyr::if_else(is.na(pfr_id), pfr_id_joined, pfr_id)) |>
      dplyr::select(-pfr_id_joined)
    saveRDS(comb, "data/nflfastR-roster.rds")
    readr::write_csv(comb, "data/nflfastR-roster.csv.gz")

    # Safe RB & FB gsis IDs for usage in RACR computation
    rb_ids <- comb |>
      dplyr::filter(position %in% c("RB", "FB"), !is.na(gsis_id)) |>
      dplyr::select(gsis_id) |>
      dplyr::distinct() |>
      dplyr::arrange(dplyr::desc(gsis_id))

    qs::qsave(rb_ids, 'data/nflfastR-RB_ids.qs',
              preset = "custom",
              algorithm = "zstd_stream",
              compress_level = 22,
              shuffle_control = 15)

    rm(list = ls())
    cli::cli_alert_info("DONE!")
  }

build_rosters()
# manual code looks like ...?
