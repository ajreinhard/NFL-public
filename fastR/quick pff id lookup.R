library(tidyverse)
library(nflfastR)
library(jsonlite)
library(magrittr)

roster_df <- fast_scraper_roster(2020)

all_qb_pff_id <- roster_df %>% 
  filter(position == 'QB' & !is.na(pff_id)) %>% 
  pull(pff_id)

qb_data <- lapply(all_qb_pff_id, function(x) {
    paste0('https://premium.pff.com/api/v1/players?league=nfl&id=', x) %>% 
      fromJSON(flatten = T) %>% 
      extract2('players')
  }) %>% 
  bind_rows %>% 
  as_tibble %>% 
  select(pff_id = id, current_eligible_year, round = draft.round, pick = draft.selection)