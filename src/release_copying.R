copy_to_releases <- function(data_loc = "data/seasons", build_folder = "build"){

x <- tibble::tibble(filename = list.files("data/seasons",pattern = "roster.*.rds",full.names = T)) |>
  dplyr::mutate(data = purrr::map(filename, ~readRDS(.x) |> nflversedata::nflverse_attrs("rosters")))

purrr::walk2(basename(x$filename) |> tools::file_path_sans_ext(),x$data,\(filename,df){

  saveRDS(df,paste0("build/",filename,".rds"))
  arrow::write_parquet(df,paste0("build/",filename,".parquet"))
  data.table::fwrite(df,paste0("build/",filename,".csv"))

})

list.files("build",full.names = T) |>
  nflversedata::nflverse_upload("rosters")

list.files("build", full.names = T) |>
  unlink()

x <- tibble::tibble(filename = list.files("data/seasons",pattern = "injuries.*.rds",full.names = T)) |>
  dplyr::mutate(data = purrr::map(filename, ~readRDS(.x) |> nflversedata::nflverse_attrs("injury and practice reports")))

purrr::walk2(basename(x$filename) |> tools::file_path_sans_ext(),x$data,\(filename,df){

  saveRDS(df,paste0("build/",filename,".rds"))
  arrow::write_parquet(df,paste0("build/",filename,".parquet"))
  data.table::fwrite(df,paste0("build/",filename,".csv"))

})

list.files("build",full.names = T) |>
  nflversedata::nflverse_upload("injuries")

list.files("build", full.names = T) |>
  unlink()

x <- tibble::tibble(filename = list.files("data/seasons",pattern = "depth_.*.rds",full.names = T)) |>
  dplyr::mutate(data = purrr::map(filename, ~readRDS(.x) |> nflversedata::nflverse_attrs("weekly depth charts")))

purrr::walk2(basename(x$filename) |> tools::file_path_sans_ext(),x$data,\(filename,df){

  saveRDS(df,paste0("build/",filename,".rds"))
  arrow::write_parquet(df,paste0("build/",filename,".parquet"))
  data.table::fwrite(df,paste0("build/",filename,".csv"))

})

list.files("build",full.names = T) |>
  nflversedata::nflverse_upload("depth_charts")

list.files("build", full.names = T) |>
  unlink()
}

copy_to_releases()
