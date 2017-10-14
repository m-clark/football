# devtools::install_github('jalapic/engsoccerdata')

library(tidyverse); library(engsoccerdata)

england2 = england1939 %>%
  mutate(Date = lubridate::date(Date)) %>%
  bind_rows(england)

mls$tier = 1

all_leagues = list(Belgium=belgium,
                   England=england2,
                   France=france,
                   Germany=germany,
                   Greece=greece,
                   Holland=holland,
                   Italy=italy,
                   `United States`=mls,
                   Portugal=portugal,
                   Scotland=scotland,
                   Spain=spain,
                   Turkey=turkey) %>%
  map(function(x) x %>%
        mutate(Date=lubridate::date(as.character(Date))) %>%
        mutate_if(is.factor, as.character)
      ) %>%
  bind_rows(.id='league') %>%
  mutate(Season = as.integer(Season)) %>%
  group_by(league) %>%
  arrange(desc(Season)) %>%
  ungroup



glimpse(all_leagues)

all_leagues_all_time_records = all_leagues %>%
  group_by(league) %>%
  do(`All Time Record` = maketable_all(.) %>%
       mutate(Pos = as.integer(Pos)))

  # filter(league=='United States') %>%
  # unnest

# longest names (for column width issues)
all_leagues %>%
  mutate(nchar_home = sapply(home, nchar),
         nchar_visitor = sapply(visitor, nchar)) %>%
  filter(nchar_home == max(nchar_home) | nchar_visitor==max(nchar_visitor))
#Association Sportive Avignonaise

enland_season_tier_tables = england %>%
  group_by(Season, tier, division) %>%
  do(tab=maketable_all(.))

save(all_leagues,
     all_leagues_all_time_records,
     enland_season_tier_tables,
     file='data/all_leagues.RData')



# all_leagues %>%
#   filter()