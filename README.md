
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflfastR-roster

This repo hosts roster data intended to be used with
[nflfastR](https://mrcaseb.github.io/nflfastR/).

We used to combine

  - the legacy roster data from the [nflfastR data
    repo](https://github.com/guga31bb/nflfastR-data/tree/master/roster-data)
    (for the seasons 1999 to 2019) and

  - weekly scraped rosters from [nfl.com](https://www.nfl.com/teams/)
    (beginning with season 2020).

However, in the meanwhile we were able to decode the special player IDs
(UUIDs version 4) in the nflfastR play-by-play data using the new
funtion `nflfastR::decode_player_ids()`. Therefore the function
`nflfastR::fast_scraper_roster()` returned to
[nflfastR](https://mrcaseb.github.io/nflfastR/) providing roster data
from the free to use [Sleeper
API](https://docs.sleeper.app/#fetch-all-players).

The following table shows an example output of all 2020 QBs of the
Kansas City Chiefs. The variable `gsis_id` can be used to join the data
to nflfastR play-by-play data.

| season | team | position | depth\_chart\_position | jersey\_number | status | full\_name      | first\_name | last\_name | birth\_date | height | weight | college      | high\_school    | gsis\_id   | espn\_id | sportradar\_id                       | yahoo\_id | rotowire\_id | update\_dt          | headshot\_url                                                                    |
| -----: | :--- | :------- | :--------------------- | -------------: | :----- | :-------------- | :---------- | :--------- | :---------- | :----- | :----- | :----------- | :-------------- | :--------- | -------: | :----------------------------------- | --------: | -----------: | :------------------ | :------------------------------------------------------------------------------- |
|   2020 | KC   | QB       | QB                     |             15 | Active | Patrick Mahomes | Patrick     | Mahomes    | 1995-09-17  | 6’3"   | 230    | Texas Tech   | Whitehouse (TX) | 00-0033873 |  3139477 | 11cad59d-90dd-449c-a839-dddaba4fe16c |     30123 |        11839 | 2020-09-24 05:30:10 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3139477.png> |
|   2020 | KC   | QB       | QB                     |              8 | Active | Matt Moore      | Matt        | Moore      | 1984-08-09  | 6’3"   | 219    | Oregon State | Hart (CA)       | 00-0025708 |    11128 | 76d7615e-8eb5-4761-b6a6-1e895d01baf3 |      8544 |         5432 | 2020-09-24 05:30:10 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/11128.png>   |
|   2020 | KC   | QB       | QB                     |              4 | Active | Chad Henne      | Chad        | Henne      | 1985-07-02  | 6’3"   | 222    | Michigan     | Wilson (PA)     | 00-0026197 |    11291 | f55053e4-4bfd-495d-981a-d62e3662f01b |      8834 |         5685 | 2020-09-24 05:30:10 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/11291.png>   |
|   2020 | KC   | QB       | QB                     |              8 | Active | Jordan Ta’amu   | Jordan      | Ta’amu     | 1997-12-10  | 6’3"   | 221    | Mississippi  | Pearl City (HI) | 00-0035735 |  4242418 | c14f8faa-62db-4fb2-a7b1-d9b5998ce604 |     32642 |        13629 | 2020-09-24 05:30:10 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4242418.png> |
|   2020 | KC   | QB       | NA                     |              9 | Active | T.J. Linta      | T.J.        | Linta      | NA          | 6’4"   | 230    | Wagner       | NA              | NA         |  3118131 | 4e2bc518-b039-4341-be4e-8385fa1265c8 |     32363 |        14183 | 2020-09-24 05:30:10 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3118131.png> |

Please note: This repo updates itself every day at 3AM ET.
