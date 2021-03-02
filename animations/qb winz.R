setwd('C:/Users/Owner/Documents/GitHub/NFL-public')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

elo_df <- get_538elo() %>% double_games()

all_weeks <- elo_df %>% 
  filter(season >= 1966) %>% 
  select(season, week) %>% 
  distinct %>% 
  mutate(week_order = row_number())

qb_wins <- elo_df %>% 
  filter(result > 0) %>% 
  select(season, week, team_qb) %>% 
  left_join(all_weeks) %>% 
  mutate(week_order = ifelse(is.na(week_order), 0, week_order)) %>% 
  full_join(all_weeks %>% select(week_order), by = character(), suffix = c('_past','')) %>%
  filter(week_order_past <= week_order & team_qb != '') %>% 
  group_by(team_qb, week_order) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  mutate(rank = row_number()) %>% 
  ungroup %>% 
  arrange(week_order, rank)
  
every_qb <- elo_df %>% filter(team_qb != '') %>% pull(team_qb) %>% unique %>% tibble %>% rename(team_qb = 1)

weekly_qb_team <- elo_df %>%
  filter(team_qb != '') %>% 
  select(season, week, team, team_qb) %>% 
  right_join(
    full_join(all_weeks, every_qb, by = character())
  ) %>%
  group_by(season, team_qb) %>% 
  fill(team, .direction = 'downup') %>%
  ungroup %>% 
  arrange(week_order)




elo_df %>%
  filter(team_qb != '') %>%
  group_by(team_qb) %>% 
  summarise(
    n = n(),
    first_yr = min(season),
    retire_yr = max(season) + 1,
  ) %>% 
  arrange(-n) %>% 
  filter(first_yr >= 1966)



qb_wins %>% 
  filter(rank <= 20 & week_order == 1) %>% 
  left_join(weekly_qb_team)

##############
qb_wins %>% 
  filter(week_order_stop == 1093) %>% 
  arrange(-n) %>% 
  slice(1:20) %>% 
  view


qb_wins %>% 
  filter(team_qb == 'John Hadl') %>% 
  arrange(week_order_stop) %>% 
  view

all_qbs <- qb_wins %>% 
  filter(rank <= 20) %>% 
  arrange(week_order_stop, rank) %>% 
  pull(team_qb)


### need to matchup with teams
### retirement teams?
### hof induction years

elo_df %>% 
  filter(team == 'CLE' & season >= 2006) %>% 
  group_by(team_qb) %>% 
  summarise(
    tot_starts = n(),
    last_start = max(gameday),
    first_start = min(gameday),
    .groups = 'drop'
  ) %>% 
  arrange(first_start) %>% 
  pull(team_qb) %>% 
  paste(collapse = ', ')
  
  
### qbs start on multiple teams
elo_df %>%
  filter(team_qb != '') %>%
  group_by(season, team_qb, team) %>% 
  summarise(starts = n()) %>% 
  mutate(teams = n()) %>% 
  filter(teams > 1 & team_qb %in% all_qbs) %>% 
  view
  


elo_df %>%
  group_by(team) %>% 
  summarise(starts = n()) %>% 
  
  

all_qbs <- qb_wins %>% 
  filter(rank <= 20) %>% 