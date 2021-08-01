source("R/git.R")
message <- sprintf("Roster updated %s (ET)", lubridate::now("America/New_York"))
git("commit", "-am", message)
