most_recent <- dplyr::if_else(
  lubridate::month(lubridate::today("GMT")) >= 9,
  lubridate::year(lubridate::today("GMT")) ,
  lubridate::year(lubridate::today("GMT")) - 1
)

message("Download pbp...")
pbp <- purrr::map_df(2011:most_recent, function(x) {readRDS(url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    ))})

message("Combine roster...")
source("combine_roster_data.R")
roster <- combine_roster_data("2020-09-10")

message("Extract player IDs from pbp...")
passer <- pbp %>%
  dplyr::mutate(join = stringr::str_extract(passer, "(?<=\\.[:space:]?)[:graph:]+(?=\\b)")) %>%
  dplyr::group_by(season, posteam, passer_id, passer, passer_jersey_number, join) %>%
  dplyr::summarise(.groups = "drop") %>%
  dplyr::filter(stringr::str_length(passer_id) > 10, !is.na(passer_jersey_number)) %>%
  dplyr::rename(pbp_id = passer_id, pbp_name = passer, jersey_number = passer_jersey_number)

receiver <- pbp %>%
  dplyr::mutate(join = stringr::str_extract(receiver, "(?<=\\.[:space:]?)[:graph:]+(?=\\b)")) %>%
  dplyr::group_by(season, posteam, receiver_id, receiver, receiver_jersey_number, join) %>%
  dplyr::summarise(.groups = "drop") %>%
  dplyr::filter(stringr::str_length(receiver_id) > 10, !is.na(receiver_jersey_number)) %>%
  dplyr::rename(pbp_id = receiver_id, pbp_name = receiver, jersey_number = receiver_jersey_number)

rusher <- pbp %>%
  dplyr::mutate(join = stringr::str_extract(rusher, "(?<=\\.[:space:]?)[:graph:]+(?=\\b)")) %>%
  dplyr::group_by(season, posteam, rusher_id, rusher, rusher_jersey_number, join) %>%
  dplyr::summarise(.groups = "drop") %>%
  dplyr::filter(stringr::str_length(rusher_id) > 10, !is.na(rusher_jersey_number)) %>%
  dplyr::rename(pbp_id = rusher_id, pbp_name = rusher, jersey_number = rusher_jersey_number)

pbp_info <- dplyr::bind_rows(passer, receiver, rusher) %>% dplyr::distinct()

message("Combine everything...")
roster_new <- roster %>%
  dplyr::select(-pbp_name) %>%
  dplyr::mutate(
    teamPlayers.birthDate = dplyr::if_else(
      team.season < 2020,
      as.Date(teamPlayers.birthDate, format = "%m/%d/%Y"),
      as.Date(teamPlayers.birthDate, format = "%Y-%m-%d")
    ),
    teamPlayers.birthDate = as.character(teamPlayers.birthDate)
  ) %>%
  dplyr::left_join(pbp_info, by = c(
    "team.season" = "season",
    "team.abbr" = "posteam",
    "join",
    "teamPlayers.jerseyNumber" = "jersey_number"
  )) %>%
  dplyr::group_by(teamPlayers.displayName, teamPlayers.birthDate) %>%
  dplyr::mutate(
    pbp_id = dplyr::first(stats::na.omit(pbp_id)),
    pbp_name = dplyr::first(stats::na.omit(pbp_name))
  )

message("Save stuff...")
saveRDS(roster_new, "data/nflfastR-roster.rds")
readr::write_csv(roster_new, "data/nflfastR-roster.csv.gz")

rm(list = ls())

message("DONE!")
