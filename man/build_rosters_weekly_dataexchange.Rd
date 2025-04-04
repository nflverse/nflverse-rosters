% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosters_dataexchange.R
\name{build_rosters_weekly_dataexchange}
\alias{build_rosters_weekly_dataexchange}
\title{Build weekly rosters from data exchange}
\usage{
build_rosters_weekly_dataexchange(
  season,
  username = Sys.getenv("NFLDX_USERNAME", "media"),
  password = Sys.getenv("NFLDX_PASSWORD", "media")
)
}
\arguments{
\item{season}{integer: season to build}
}
\value{
dataframe of weekly rosters
}
\description{
Build weekly rosters from data exchange
}
