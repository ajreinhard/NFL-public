source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(ggstance)
library(ggtext)

complete_box <- readRDS('pfr/data/boxscores.rds') %>%
  filter(season >= 1964 & game_type == 'REG')

all_weeks <- complete_box %>% 
  select(season, week) %>% 
  distinct %>% 
  mutate(week_order = row_number())

team_sched <- complete_box %>% 
  select(season, week, team) %>% 
  mutate(game_week = 1) %>% 
  distinct

all_player_weeks <- complete_box %>% 
  left_join(all_weeks) %>% 
  group_by(player_id) %>% 
  summarise(week_order = (min(week_order)-1):(max(week_order) + 17)) %>% 
  ungroup %>% 
  left_join(all_weeks) %>% 
  left_join(complete_box) %>% 
  group_by(player_id, season) %>% 
  fill(team, .direction = 'downup') %>% 
  group_by(player_id) %>% 
  fill(team, .direction = 'downup') %>% 
  left_join(team_sched) %>% 
  mutate_at(.vars = vars(contains(c('game_week','pass','rush','rec'))), function(x) cumsum(ifelse(is.na(x), 0, x))) %>% 
  ungroup


compare_17ago <- function(fld_name) !!as.name(paste0(fld_name))# - !!as.name(paste0(fld_name,'_17ago'))
compare_17ago <- function(fld_name) glue('{',fld_name,'}') - glue('{',fld_name,'_17ago}')
trailing_cols <- all_player_weeks %>% select(contains(c('pass','rush','rec'))) %>% names

trailing_df <- all_player_weeks %>% 
  select(player_id, game_week_17ago = game_week, contains(c('pass','rush','rec'))) %>% 
  distinct %>% 
  right_join(
    all_player_weeks %>%
      mutate(game_week_17ago = ifelse(game_week < 18, 0, game_week - 17)),
    by = c('player_id', 'game_week_17ago'),
    suffix = c('_17ago', '')
  ) %>% 
  mutate(
    rec_trl17 = rec - rec_17ago,
    rec_yds_trl17 = rec_yds - rec_yds_17ago,
    rec_td_trl17 = rec_td - rec_td_17ago
    #rec_trl_17 = compare_17ago(rec)
    
    #rec2 = glue('{rec}') - glue('{rec_17ago}')
    #across(.cols = trailing_cols, .fns = function(x) glue('{cur_column()}'), .names = '{.col}_trail17')
    #rec2 = compare_17ago('rec')
    #across(.cols = trailing_cols, .fns = compare_17ago, .names = '{.col}_trail17')
    #ok = !!as.name('{{ trailing_cols }}')
    #'{trailing_cols}_new' := !!as.name('{{ trailing_cols }}') - !!as.name('{{ trailing_cols }}_17ago')
    #rec_trl_17 = compare_17ago('rec')
    #rec_trl_17 = !!as.name(paste0('rec'))
    #across(.cols = ends_with('_17ago'), .fns = function(x) cur_column(), .names = '{.col}_trail17')
    #across(.cols = ends_with('_17ago'), .fns = compare_17ago, .names = '{.col}_trail17')
  )
  

trailing_df %>% arrange(-rec_trl_17)


trailing_df %>%
  rename(focal_stat = rec_yds_trl17) %>% 
  arrange(-focal_stat, week_order) %>%
  slice(1:10000) %>%
  group_by(player_id, focal_stat, game_week) %>%
  summarise(each_week_order = game_week:game_week_17ago) %>%
  ungroup %>%
  group_by(player_id, each_week_order) %>% 
  mutate(max_yrds = max(focal_stat)) %>% 
  ungroup %>% 
  group_by(player_id, focal_stat, game_week) %>% 
  summarise(max_max_yrds = max(max_yrds)) %>% 
  filter(max_max_yrds == focal_stat & row_number() == 1) %>% 
  ungroup %>% 
  arrange(-max_max_yrds) %>% 
  #arrange(player_id, game_week) %>% 
  #filter(player_id == 'PeteAd01')
  view




?Reduce
packageVersion('dplyr')
trailing_df$rec_trl_17
library(glue)

all_player_weeks %>% 
  select(rec) %>% 
  mutate(rec2 = compare_17ago('rec'))

?quo_name()
?across
  #mutate(trailing17_yrds = car_rush_yds - car_rush_yds_17ago)
#paste0(gsub('_17ago', '', cur_column()))
trailing_df$pass_cmp_17ago_trail17
#
trailing_df$rec_17ago
?vars

trailing_df %>% filter(player_id == 'AbduAm00') %>% view

trailing_df %>% arrange(-trailing17_yrds)

trailing_df %>% 
  filter(player_id == 'PeteAd01') %>% 
  view




trailing_df %>%
  arrange(-trailing17_yrds, week_order) %>%
  slice(1:10000) %>%
  group_by(player_id, trailing17_yrds, game_week) %>%
  summarise(each_week_order = game_week:game_week_17ago) %>%
  ungroup %>%
  group_by(player_id, each_week_order) %>% 
  mutate(max_yrds = max(trailing17_yrds)) %>% 
  ungroup %>% 
  group_by(player_id, trailing17_yrds, game_week) %>% 
  summarise(max_max_yrds = max(max_yrds)) %>% 
  ungroup %>% 
  filter(max_max_yrds == trailing17_yrds) %>% 
  arrange(player_id, game_week) %>% 
  #filter(player_id == 'PeteAd01')
  view



