#### Fill player IDs based on their ID for any future or past season ####
# Code by Tan Ho (@_TanHo on Twitter, tanho63 on GitHub)

`%>%` <- magrittr::`%>%`
`%T>%` <- magrittr::`%T>%`

original_rosters <- readRDS("data/nflfastR-roster.rds")

filled_rosters <- readRDS("data/nflfastR-roster.rds") %>%
  dplyr::left_join(manual_patch, by = c("gsis_id")) %>%
  dplyr::mutate(sportradar_id = dplyr::coalesce(sportradar_id.x,sportradar_id.y),
                sportradar_id.x = NULL,
                sportradar_id.y = NULL) %>%
  dplyr::filter(!is.na(gsis_id)) %>%
  dplyr::group_by(gsis_id) %>%
  tidyr::fill(
    dplyr::contains("id"),
    birth_date,
    height,
    weight,
    college,
    high_school,
    headshot_url,
    .direction = "updown") %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(original_rosters %>% dplyr::filter(is.na(gsis_id)))

saveRDS(filled_rosters, "data/nflfastR-roster.rds")
readr::write_csv(filled_rosters, "data/nflfastR-roster.csv.gz")

filled_rosters %>%
  dplyr::group_by(season) %T>%
  dplyr::group_map(~saveRDS(.x,file = glue::glue("data/seasons/roster_{.y$season}.rds")),.keep = TRUE) %T>%
  dplyr::group_map(~readr::write_csv(.x,file = glue::glue("data/seasons/roster_{.y$season}.csv")), .keep = TRUE) %>%
  invisible()
