
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflfastR-roster

This repo hosts roster data which can be accessed with
[nflreadr](https://nflreadr.nflverse.com).

You can see the status of this repository on the [nflverse status
page](https://github.com/nflverse/status).

This data is primarily sourced from the NFL.com API and supplemented by
the free-to-use [Sleeper
API](https://docs.sleeper.app/#fetch-all-players).

Here is an example of using this data!

``` r
library(dplyr)
library(nflreadr)
nflreadr::load_rosters(2020) %>% 
  dplyr::filter(team == "KC", position == "QB") 
#> # A tibble: 6 x 25
#>   season team  position depth_chart_position jersey_number status full_name     
#>    <dbl> <chr> <chr>    <chr>                        <int> <chr>  <chr>         
#> 1   2020 KC    QB       QB                              15 Active Patrick Mahom~
#> 2   2020 KC    QB       <NA>                             8 Active Matt Moore    
#> 3   2020 KC    QB       QB                               4 Active Chad Henne    
#> 4   2020 KC    QB       <NA>                             8 Active Jordan Ta'amu 
#> 5   2020 KC    QB       <NA>                             6 Active Anthony Gordon
#> 6   2020 KC    QB       <NA>                             9 Active T.J. Linta    
#> # ... with 18 more variables: first_name <chr>, last_name <chr>,
#> #   birth_date <date>, height <chr>, weight <chr>, college <chr>,
#> #   high_school <chr>, gsis_id <chr>, espn_id <int>, yahoo_id <int>,
#> #   rotowire_id <int>, pff_id <int>, pfr_id <chr>, headshot_url <chr>,
#> #   fantasy_data_id <int>, sleeper_id <chr>, years_exp <int>,
#> #   sportradar_id <chr>
```
