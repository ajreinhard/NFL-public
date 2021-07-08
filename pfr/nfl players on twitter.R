library(rvest)
library(tidyverse)
library(rtweet)

twit_html <- read_html('https://www.pro-football-reference.com/friv/nfl-player-twitter.htm')

player_id <- twit_html %>%
  html_nodes(xpath = '//div[@class="section_content"]/p/a[1]/@href') %>%
  html_text %>% 
  gsub('.htm','',.) %>% 
  tibble %>% 
  rename(id = 1) %>% 
  separate(id, '/', into = c(NA, NA, NA, 'player_id'))

twitter_handle <- twit_html %>%
  html_nodes(xpath = '//div[@class="section_content"]/p/a[2]') %>% 
  html_text %>% 
  tibble %>% 
  rename(handle = 1)

handle_df <- bind_cols(player_id, twitter_handle) %>% 
  filter(!is.na(player_id) & twitter_handle != '')

nfl_twitter_df <- handle_df %>% 
  pull(handle) %>% 
  lookup_users

nfl_twitter_df %>% 
  select(screen_name, verified, followers_count, friends_count) %>%
  filter(followers_count >= 1000 | verified) %>% 
  arrange(-friends_count)



team_html <- read_html('https://www.pro-football-reference.com/teams/crd/2021_roster.htm')
team_html %>% 
  html_nodes(xpath = '//table//@data-append-csv')
