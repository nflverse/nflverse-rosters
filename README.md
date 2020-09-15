
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflfastR-roster

This repo hosts roster data intended to be used with
[nflfastR](https://mrcaseb.github.io/nflfastR/).

The data is a combination of

  - the legacy roster data from the [nflfastR data
    repo](https://github.com/guga31bb/nflfastR-data/tree/master/roster-data)
    (for the seasons 1999 to 2019) and

  - weekly scraped rosters from [nfl.com](https://www.nfl.com/teams/)
    (beginning with season 2020).

For that reason many variables will show `NA` beginning in 2020.

The nflfastR play-by-play data uses special player IDs (UUIDs version
4). The roster data in this repo joins those IDs to the scraped rosters.
Therefore only players who appear as `passer`, `rusher`, or `receiver`
in the nflfastR play-by-play data will show the `pbp_id`. This ID can be
used to join the roster data to the play-by-play data.

The following table shows an example output of the 2020 data for Patrick
Mahomes. The variables `pbp_id` and `pbp_name` are joinable to nflfastR.

| team.season | team.abbr | teamPlayers.displayName | teamPlayers.position | teamPlayers.birthDate | teamPlayers.collegeName | teamPlayers.height | teamPlayers.weight | pbp\_id                              | pbp\_name |
| ----------: | :-------- | :---------------------- | :------------------- | :-------------------- | :---------------------- | :----------------- | -----------------: | :----------------------------------- | :-------- |
|        2020 | KC        | Patrick Mahomes         | QB                   | 1995-09-17            | Texas Tech              | 6-3                |                230 | 32013030-2d30-3033-3338-3733fa30c4fa | P.Mahomes |

Please note: This repo updates itself every 15 minutes by joining the
most recent [nflfastR play-by-play
data](https://github.com/guga31bb/nflfastR-data/tree/master/data) to the
scraped rosters. So if a very new player ID is missing please wait until
the data repo is updated.
