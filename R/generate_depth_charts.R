seasons_to_scrape = c(2001:(lubridate::year(Sys.Date())))

message('Scraping teams...')

scrape_teams = function(year){
  h = httr::handle('https://www.nfl.info')
  r = httr::GET(handle = h,
                path=glue::glue('/nfldataexchange/dataexchange.asmx/getClubs?lseason={year}'),
                httr::authenticate('media','media'),
                url=NULL)
  teams_df = httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h) # close handle when finished, have had the api get mad when I don't close it
  return(teams_df)
}

teams = purrr::map_dfr(seasons_to_scrape, scrape_teams) |>
  dplyr::filter(!(ClubCode %in% c('AFC','NFC','RIC','SAN','CRT','IRV'))) |> # remove all-star teams
  dplyr::mutate(Season = as.integer(Season)) |>
  dplyr::select(club_code = ClubCode, season = Season) |>
  tidyr::expand_grid(season_type = c('REG','POST'))

message('Scraping depth charts...')

scrape_dc = function(season, team, season_type){
  # message(glue::glue("Scraping {team}'s depth charts from the {season} {season_type} season..."))
  h = httr::handle('https://www.nfl.info')
  r = httr::GET(handle = h,
                path=glue::glue('/nfldataexchange/dataexchange.asmx/getGameDepthChart?lSeason={season}&lSeasonType={season_type}&lWeek=0&lClub={team}'),
                httr::authenticate('media','media'),
                url=NULL)
  dc_df = httr::content(r) |>
    XML::xmlParse() |>
    XML::xmlToDataFrame()
  rm(h)
  return(dc_df)
}

dc_df = purrr::pmap_dfr(list(teams$season,teams$club_code,teams$season_type), scrape_dc) |>
  dplyr::mutate(ClubCode = dplyr::case_when(ClubCode == 'ARZ'~'ARI',
                                            ClubCode == 'BLT'~'BAL',
                                            ClubCode == 'CLV'~'CLE',
                                            ClubCode == 'HST'~'HOU',
                                            ClubCode == 'SL'~'STL',
                                            T~ClubCode),
                full_name = paste(FootballName, LastName),
                across(c(Season, Week, DepthTeam, JerseyNumber), as.integer)) |>
  dplyr::select(season = Season,
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
                gsis_id = GsisID)

message('Saving depth charts...')
dc_split = dc_df |> dplyr::group_split(season)

purrr::walk(dc_split, function(x){
  saveRDS(x, glue::glue('data/seasons/dc_{unique(x$season)}.rds'))
  readr::write_csv(x, glue::glue('data/seasons/dc_{unique(x$season)}.csv.gz'))
  # qs::qsave(x, glue::glue('data/seasons/dc_{unique(x$season)}.qs'),
  #           preset = "custom",
  #           algorithm = "zstd_stream",
  #           compress_level = 22,
  #           shuffle_control = 15)
})

saveRDS(dc_df,'data/nflfastR-dc.rds')
readr::write_csv(dc_df, 'data/nflfastR-dc.csv.gz')
