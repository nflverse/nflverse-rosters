`%>%`<-magrittr::`%>%`

combine_roster_data <- function(start_date) {
  start <- lubridate::as_date(start_date) # Thursaday Week 1 2020
  weeks <- lubridate::days(lubridate::today("UTC")-start) %>%
    .$day %>%
    magrittr::divide_by_int(7)

  roster_raw <- purrr::map_dfr(0:weeks, function(x, s){
    day <- s + lubridate::dweeks(x)
    filename <- glue::glue('{lubridate::year(day)}_{format.Date(day, "%m")}_{format.Date(day, "%d")}_roster.rds?raw=true')
    path <- glue::glue("https://github.com/guga31bb/nflfastR-raw/blob/master/roster/{filename}")
    return(readRDS(url(path)))
  }, start) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ind = 1:dplyr::n())

  dates <- roster_raw %>% dplyr::select(ind, scrape_day, scrape_point)

  rem_dupl <- roster_raw %>%
    dplyr::select(-c("scrape_day", "scrape_point")) %>%
    dplyr::na_if("") %>%
    dplyr::select("ind", "team_abbr", "player", "no", "pos") %>%
    dplyr::group_by(team_abbr, player, no) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull("ind")

  roster <- roster_raw %>%
    dplyr::select(-c("scrape_day", "scrape_point")) %>%
    dplyr::na_if("") %>%
    dplyr::filter(ind %in% rem_dupl) %>%
    dplyr::left_join(dates, by = "ind") %>%
    dplyr::mutate(
      teamPlayers.firstName = player %>% stringr::str_split_fixed(" ", 2) %>% .[,1],
      teamPlayers.lastName = player %>% stringr::str_split_fixed(" ", 2) %>% .[,2]
      # team.nick = team_name %>% stringr::str_split_fixed(" ", 2) %>% .[,2]
      # teamPlayers.suffix = teamPlayers.lastName %>% stringr::str_split_fixed(" ", 2) %>% .[,2]
      # name = glue::glue("{stringr::str_sub(teamPlayers.firstName, 1, 1)}.{teamPlayers.lastName}")
    ) %>%
    dplyr::select(
      team.season = season,
      teamPlayers.displayName = player,
      teamPlayers.firstName,
      teamPlayers.lastName,
      teamPlayers.status = status,
      teamPlayers.position = pos,
      teamPlayers.birthDate = birthdate,
      teamPlayers.collegeName = college,
      teamPlayers.jerseyNumber = no,
      teamPlayers.height = height,
      teamPlayers.weight = weight,
      team.abbr = team_abbr,
      team.fullName = team_name,
      # team.nick,
      scrape_dt = scrape_point
    ) %>%
    dplyr::arrange(team.season, team.abbr)

  out <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>%
    dplyr::bind_rows(roster) %>%
    dplyr::mutate(
      pbp_name = glue::glue("{stringr::str_sub(teamPlayers.firstName, 1, 1)}.{teamPlayers.lastName}") %>% as.character(),
      pbp_name = dplyr::case_when(
        teamPlayers.displayName == "Robert Griffin III" ~ "R.Griffin III",
        teamPlayers.displayName == "Gardner Minshew" ~ "G.Minshew II",
        TRUE ~ pbp_name
      ),
      join = stringr::str_extract(pbp_name, "(?<=\\.)[:graph:]+(?=\\b)"),
      team.abbr = dplyr::case_when(
        team.abbr == "STL" ~ "LA",
        team.abbr == "SD" ~ "LAC",
        team.abbr == "OAK" ~ "LV",
        TRUE ~ team.abbr
      )
    ) %>%
    dplyr::select(team.season, team.abbr, pbp_name, teamPlayers.jerseyNumber, dplyr::everything())

  return(out)
}
