library(tidyverse)
library(nflreadr)

### get position and full name of all players from 2021
name_position_df <- load_rosters(2021) %>%
  filter(!is.na(gsis_id)) %>%
  select(gsis_id, full_name, position)

### table of player-team pairs that have appeared on an NFL depth chart
### Tom Brady is the only active player that would have been listed on a pre-01 depth chart, adding those years
hist_depth_df <- load_depth_charts(2001:2021) %>%
  mutate(
    team = case_when(
      team == 'STL' ~ 'LA',
      team == 'SD' ~ 'LAC',
      team == 'OAK' ~ 'LV',
      TRUE ~ team
    )
  ) %>% 
  select(season, team, gsis_id) %>% 
  distinct %>% 
  bind_rows(
    tibble(
      season = 1999:2000,
      team = 'NE',
      gsis_id = '00-0019596'
    )
  )

### a table with one row for every player-week
all_player_weeks_df <- load_depth_charts(2021) %>% 
  group_by(season, gsis_id) %>% 
  summarise(week = 1:22, .groups = 'drop')

### current depth charts
current_depth_df <- load_depth_charts(2021) %>% 
  group_by(team) %>% 
  filter(week == max(week)) %>% 
  ungroup %>% 
  select(season, team, gsis_id) %>% 
  distinct %>% 
  mutate(current_status = 'on_depth_chart') 
  
### a table with one row for every player week and the team they were probably on
### weekly_status = were they on the depth chart for that team in that week?
### current_status = are they on the depth chart for that team in the most recent update?
current_season_weekly_roster_df <- load_depth_charts(2021) %>% 
  select(season, week, team, gsis_id) %>% 
  mutate(weekly_status = 'on_depth_chart') %>% 
  right_join(all_player_weeks_df) %>% 
  group_by(gsis_id) %>% 
  arrange(week) %>% 
  fill(team, .direction = 'downup') %>% 
  ungroup %>% 
  left_join(current_depth_df) %>% 
  mutate(
    weekly_status = ifelse(is.na(weekly_status), 'missing', weekly_status),
    current_status = ifelse(is.na(current_status), 'missing', current_status)
  ) %>% 
  left_join(name_position_df)

### 2021 schedule, doubled so that each game is listed for both teams
sched_df <- bind_rows(
  load_schedules(2021) %>% select(game_id, week, team = home_team, opp = away_team),
  load_schedules(2021) %>% select(game_id, week, team = away_team, opp = home_team)
)

### join weekly roster to schedule and join the sched opp to historical roster
revenge_df <- current_season_weekly_roster_df %>% 
  select(-season) %>% 
  left_join(sched_df) %>% 
  inner_join(hist_depth_df, by = c('opp' = 'team', 'gsis_id')) %>% 
  rename(former_team = opp, season_former_team = season) %>% 
  group_by(across(c(-season_former_team))) %>% 
  summarise(
    years_with_team = n(),
    last_with_team = max(season_former_team),
    .groups = 'drop'
  )


write.csv(revenge_df, 'misc-data/2021-revenge-games.csv', row.names = F)




######### Revenge Game Data Creation is complete
### create the revenge games graphic
### need my source file to create the graph stuff
my_week = 8
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

### df that will give team nicknames
### I prefer to use Washingon & Kansas City as nicknames
team_df <- teams_colors_logos %>% 
  mutate(
    team_city = substr(team_name, 1, nchar(team_name) - nchar(team_nick) - 1),
    team_nick = ifelse(team_abbr %in% c('WAS', 'KC'), team_city, team_nick)
  )

### use this to create nicknames for games and set order by time
full_sched_df <- load_schedules(2021) %>% 
  left_join(team_df, by = c('home_team' = 'team_abbr')) %>% 
  left_join(team_df, by = c('away_team' = 'team_abbr'), suffix = c('_home', '_away')) %>% 
  group_by(week) %>% 
  mutate(game_ord = row_number()) %>% 
  ungroup %>% 
  mutate(game_name = paste0(team_nick_away, ' @ ', team_nick_home))

### get the helmets for the background
team_helms <- sched_df %>% 
  left_join(full_sched_df) %>% 
  mutate(
    home_away = ifelse(team == home_team, 'home', 'away'),
    helm_url = ifelse(team == home_team, helm2020(team, 'R'), helm2020(team, 'L')),
    player_order = 3
  ) %>% 
  filter(week == my_week) %>% 
  select(game_id, home_away, helm_url, player_order) %>% 
  left_join(full_sched_df) %>% 
  mutate(game_name = factor(game_name, unique(game_name)))

### use only players listed currently on the depth chart
p <- revenge_df %>% 
  filter(week == my_week & current_status == 'on_depth_chart') %>%
  left_join(full_sched_df) %>% 
  mutate(
    home_away = ifelse(team == home_team, 'home', 'away'),
    player_desc = paste0(full_name,', ', position),
    rev_info = paste0(years_with_team, 'yr', ifelse(years_with_team > 1, 's', ''), ' - Last in ', last_with_team)
  ) %>% 
  arrange(-years_with_team, -last_with_team) %>% 
  group_by(game_id, team) %>% 
  mutate(player_order = row_number()) %>% 
  filter(player_order < 5) %>% 
  ungroup %>% 
  arrange(game_ord) %>% 
  mutate(game_name = factor(game_name, levels(team_helms$game_name))) %>% 
  ggplot(aes(x = home_away, y = player_order)) +
  facet_wrap(.~game_name, nrow = 4, ncol = 4) +
  geom_image(data = team_helms, aes(image = helm_url), size = 0.4, asp = 26/9) +
  geom_image(data = team_helms, aes(image = helm2020('CLE', ifelse(home_away == 'home', 'R', 'L'))), size = 0.4, asp = 26/9, color = 'white', alpha = 0.7) +
  geom_shadowtext(aes(label = rev_info), position = position_nudge(y = -0.4), family = font_SB, size = 1.6, color = 'darkblue', bg.color = 'grey95', bg.r = 0.25) +
  geom_shadowtext(aes(label = player_desc), family = font_SB, size = 1.6, color = 'darkblue', bg.color = 'grey95', bg.r = 0.25) +
  labs(title = paste0('Revenge Games, 2021 Week ', my_week),
       x = NULL,
       y = NULL
  ) +
  scale_y_reverse(limits = c(5, 1), expand = expansion(add = c(0, 0.2))) +
  theme_SB +
  theme(
    strip.text = element_text(size = 9),
    panel.spacing.y = unit(3, 'pt'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

brand_plot(p, asp = 16/9, save_name = 'graphs/revenge games.png', data_home = 'Data: nflreadr', base_size = 4.3)

