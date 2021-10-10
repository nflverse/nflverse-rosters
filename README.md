
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflfastR-roster

This repo hosts roster data which can be accessed with
[nflreadr](https://nflreadr.nflverse.com).

This data is primarily sourced from the NFL.com API and supplemented by
the free-to-use [Sleeper
API](https://docs.sleeper.app/#fetch-all-players).

Here is an example of using this data!

``` r
library(dplyr)
library(nflreadr)
nflreadr::load_rosters(2020) %>% 
  dplyr::filter(team == "KC", position == "QB") 
```

| season | team | position | depth\_chart\_position | jersey\_number | status | full\_name      | first\_name | last\_name | birth\_date | height | weight | college          | high\_school    | gsis\_id   | espn\_id | yahoo\_id | rotowire\_id | pff\_id | pfr\_id  | headshot\_url                                                                    | fantasy\_data\_id | sleeper\_id | years\_exp | sportradar\_id                       |
|-------:|:-----|:---------|:-----------------------|---------------:|:-------|:----------------|:------------|:-----------|:------------|:-------|:-------|:-----------------|:----------------|:-----------|---------:|----------:|-------------:|--------:|:---------|:---------------------------------------------------------------------------------|------------------:|:------------|-----------:|:-------------------------------------|
|   2020 | KC   | QB       | QB                     |             15 | Active | Patrick Mahomes | Patrick     | Mahomes    | 1995-09-17  | 6-3    | 230    | Texas Tech       | Whitehouse (TX) | 00-0033873 |  3139477 |     30123 |        11839 |   11765 | MahoPa00 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3139477.png> |             18890 | 4046        |         NA | 11cad59d-90dd-449c-a839-dddaba4fe16c |
|   2020 | KC   | QB       | NA                     |              8 | Active | Matt Moore      | Matt        | Moore      | 1984-08-09  | 6-3    | 219    | Oregon State     | Hart (CA)       | 00-0025708 |    11128 |      8544 |         5432 |    4018 | MoorMa01 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/11128.png>   |              5834 | 233         |         NA | 76d7615e-8eb5-4761-b6a6-1e895d01baf3 |
|   2020 | KC   | QB       | QB                     |              4 | Active | Chad Henne      | Chad        | Henne      | 1985-07-02  | 6-3    | 222    | Michigan         | Wilson (PA)     | 00-0026197 |    11291 |      8834 |         5685 |    4371 | HennCh01 | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/11291.png>   |              2405 | 89          |         NA | f55053e4-4bfd-495d-981a-d62e3662f01b |
|   2020 | KC   | QB       | NA                     |              8 | Active | Jordan Ta’amu   | Jordan      | Ta’amu     | 1997-12-10  | 6-3    | 221    | Mississippi      | Pearl City (HI) | 00-0035735 |  4242418 |     32642 |        13629 |   60453 | NA       | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4242418.png> |             21612 | 6730        |         NA | c14f8faa-62db-4fb2-a7b1-d9b5998ce604 |
|   2020 | KC   | QB       | NA                     |              6 | Active | Anthony Gordon  | Anthony     | Gordon     | 1997-08-28  | 6-3    | 210    | Washington State | Terra Nova (CA) | NA         |  4055171 |     33266 |        14431 |      NA | NA       | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4055171.png> |             21816 | 6898        |         NA | 71a2444a-a6b1-4e62-8974-2d858e2c5b73 |
|   2020 | KC   | QB       | NA                     |              9 | Active | T.J. Linta      | T.J.        | Linta      | NA          | 6-4    | 230    | Wagner           | NA              | NA         |  3118131 |     32363 |        14183 |      NA | NA       | <https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3118131.png> |             21372 | 6522        |         NA | 4e2bc518-b039-4341-be4e-8385fa1265c8 |

You can see the status of this repository on the [nflverse status
page](https://github.com/nflverse/status).
