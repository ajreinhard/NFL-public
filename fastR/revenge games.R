source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

my_week = 6

roster_df <- load_rosters(1999:2021) %>%
  filter(!is.na(gsis_id)) %>% 
  mutate(
    team = case_when(
      team == 'STL' ~ 'LA',
      team == 'SD' ~ 'LAC',
      team == 'OAK' ~ 'LV',
      TRUE ~ team
    )
  )

sched_df <- bind_rows(
  load_schedules(2021) %>% select(game_id, team = home_team, opp = away_team),
  load_schedules(2021) %>% select(game_id, team = away_team, opp = home_team)
)

revenge_df <- roster_df %>% 
  filter(season == 2021) %>%
  select(team, gsis_id) %>% 
  left_join(sched_df, by = 'team') %>% 
  inner_join(roster_df, by = c('opp' = 'team', 'gsis_id')) %>% 
  select(gsis_id, game_id, current_team = team, former_team = opp, season_former_team = season) %>%
  group_by(gsis_id, current_team, former_team, game_id) %>% 
  summarise(
    years_with_team = n(),
    last_with_team = max(season_former_team),
    .groups = 'drop'
  )

team_df <- teams_colors_logos %>% 
  mutate(
    team_city = substr(team_name, 1, nchar(team_name) - nchar(team_nick) - 1),
    team_nick = ifelse(team_abbr %in% c('WAS', 'KC'), team_city, team_nick)
  )

full_sched_df <- load_schedules(2021) %>% 
  left_join(team_df, by = c('home_team' = 'team_abbr')) %>% 
  left_join(team_df, by = c('away_team' = 'team_abbr'), suffix = c('_home', '_away')) %>% 
  group_by(week) %>% 
  mutate(game_ord = row_number()) %>% 
  ungroup %>% 
  mutate(game_name = paste0(team_nick_away, ' @ ', team_nick_home))

on_depth_chart <- load_depth_charts() %>%
  filter(season == 2021 & week == my_week) %>%
  select(current_team = team, gsis_id) %>%
  distinct

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
  
p <- revenge_df %>% 
  inner_join(roster_df %>% filter(season == 2021)) %>%
  inner_join(on_depth_chart) %>% 
  left_join(full_sched_df) %>% 
  mutate(
    home_away = ifelse(current_team == home_team, 'home', 'away'),
    player_desc = paste0(full_name,', ', position),
    rev_info = paste0(years_with_team, 'yr', ifelse(years_with_team > 1, 's', ''), ' - Last in ', last_with_team)
  ) %>% 
  arrange(-years_with_team, -last_with_team) %>% 
  group_by(game_id, current_team) %>% 
  mutate(player_order = row_number()) %>% 
  ungroup %>% 
  filter(week == my_week) %>% 
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

