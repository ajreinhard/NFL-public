devtools::source_url('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- load_pbp(2021)
games_df <- load_schedules(2022) %>% double_games

team_ranks <- pbp_df %>% 
  filter(!is.na(down) & pass + rush == 1 & season_type == 'REG') %>% 
  select(teamoff = posteam, teamdef = defteam, pass, epa) %>% 
  pivot_longer(contains('team'), names_to = 'side', values_to = 'team', names_prefix = 'team') %>% 
  group_by(team, side, pass) %>% 
  summarise(epa_play = mean(epa, na.rm = T)) %>% 
  mutate(epa_play = ifelse(side == 'def', -epa_play, epa_play)) %>% 
  group_by(side, pass) %>% 
  arrange(-epa_play) %>% 
  mutate(facet_rank = row_number()) %>% 
  ungroup

thermo_df <- expand.grid(facet_rank = 1:32, week = 1:18, pass = c(0,1), side = c('off', 'def')) %>% 
  mutate(
    game_facet = case_when(
      pass == 1 & side == 'off' ~ 'Passing Offense',
      pass == 0 & side == 'off' ~ 'Rushing Offense',
      pass == 1 & side == 'def' ~ 'Passing Defense',
      pass == 0 & side == 'def' ~ 'Rushing Defense'
    ),
    game_facet = factor(game_facet, rev(c('Passing Offense', 'Rushing Offense', 'Passing Defense', 'Rushing Defense')))
  )

p <- 'CLE' %>% 
  expand.grid(team = ., week = 1:18, pass = c(0,1), side = c('off', 'def')) %>% 
  left_join(games_df) %>% 
  left_join(team_ranks, by = c('opp' = 'team', 'pass', 'side')) %>% 
  separate(gametime, sep = ':', into = c('game_hr','game_mn')) %>% 
  mutate(
    game_facet = case_when(
      pass == 1 & side == 'off' ~ 'Passing Offense',
      pass == 0 & side == 'off' ~ 'Rushing Offense',
      pass == 1 & side == 'def' ~ 'Passing Defense',
      pass == 0 & side == 'def' ~ 'Rushing Defense'
    ),
    game_facet = factor(game_facet, rev(c('Passing Offense', 'Rushing Offense', 'Passing Defense', 'Rushing Defense'))),
    gameday = as.Date(gameday),
    date_txt = paste0(format(gameday, '%a'), ' ', as.numeric(game_hr) - 12, ':', game_mn, ifelse(game_hr < 12, 'am', 'pm')),
    date_txt = ifelse(is.na(opp), NA, date_txt),
    win_loss = ifelse(result == 0, 'T', ifelse(result > 0, 'W', 'L')),
    game_score = paste0(win_loss, ' ', team_score, '-', opp_score),
    home_away = ifelse(location == 'Away', '@', '')
  ) %>% 
  rename(act_team = team, team = opp) %>% 
  hist_logo_url %>% 
  rename(opp = team, team = act_team) %>% 
  ggplot(aes(x = facet_rank, y = game_facet)) +
  facet_wrap(.~week, nrow = 3) +
  geom_text(aes(label = ifelse(game_facet == 'Passing Offense', paste0('Week ',week), NA), x = 1), size = 3.3, color = 'darkblue', position = position_nudge(y = 1.3), family = font_SB, hjust = 0) +
  geom_text(aes(label = ifelse(game_facet == 'Passing Offense', date_txt, NA), x = 1), size = 2.2, color = 'darkblue', position = position_nudge(y = 0.8), family = font_SB, hjust = 0) +
  geom_text(aes(label = ifelse(game_facet == 'Passing Offense', home_away, NA), x = 22), size = 2.5, color = 'darkblue', position = position_nudge(y = 1.2), family = font_SB, hjust = 0) +
  geom_line(data = thermo_df, color = 'grey30', size = 3, lineend = 'round') +
  geom_line(data = thermo_df, aes(color = facet_rank), size = 2, lineend = 'round', show.legend = F) +
  geom_line(data = thermo_df, aes(color = facet_rank), size = 2, show.legend = F) +
  geom_point(size = 3, shape = 24, fill = 'gold', color = 'grey30', position = position_nudge(y = -0.3)) +
  geom_text(aes(label = facet_rank), size = 1.7, color = 'grey20', position = position_nudge(y = -0.3), family = font_SB) +
  geom_text(aes(label = game_facet, x = 2), size = 2, color = 'grey30', position = position_nudge(y = 0.35), family = font_SB, hjust = 0) +
  geom_rect(aes(xmin = ifelse(is.na(facet_rank), -Inf, NA), xmax = Inf, ymin = 'Passing Offense', ymax = -Inf), position = position_nudge(y = 0.5), color = 'grey95', fill = 'grey95') +
  geom_text(aes(x = ifelse(is.na(facet_rank), 16.5, NA), y = 'Passing Defense', label = 'BYE'), position = position_nudge(y = 0.75), color = 'grey50', angle = 30, size = 13, family = font_SB) +
  # geom_rect(aes(xmin = ifelse(!is.na(win_loss), -Inf, NA), xmax = Inf, ymin = 'Passing Offense', ymax = -Inf), position = position_nudge(y = 0.5), color = 'grey95', fill = 'grey95', alpha = 0.2) +
  # geom_shadowtext(aes(x = ifelse(!is.na(win_loss), 16.5, NA), y = 'Passing Defense', label = game_score), position = position_nudge(y = 0.75), color = 'grey50', angle = 20, size = 9, family = font_SB, bg.r = 0.15, bg.color = 'grey95') + 
  geom_image(aes(image = logo_url, x = 28, y = 'Passing Offense'), size = 0.2, position = position_nudge(y = 0.9), asp = 1.07) +
  labs(title = '2022 Cleveland Browns Schedule',
       subtitle = '2021 EPA/Play Rankings by Offense/Defense & Passing/Rushing',
       x = NULL,
       y = NULL) +
  scale_x_continuous(expand = expansion(add = 3)) +
  scale_y_discrete(expand = expansion(add = c(0.7,1.5))) +
  scale_color_gradient2(low = 'darkorange', mid = 'white', high = '#47a0ff', midpoint = 16) +
  theme_SB +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_blank(),
    panel.spacing.x = unit(0.3, 'lines'),
    panel.spacing.y = unit(0.4, 'lines')
  )

brand_plot(p, asp = 16/9, save_name = 'browns sched 2022 opp ranks.png', data_home = 'Data: @nflfastR', base_size = 4.5)


# team ranking text
# 
# team_ranks %>%
#   filter(team == 'CLE') %>% 
#   arrange(desc(side), -pass) %>% 
#   mutate(
#     game_facet = case_when(
#       pass == 1 & side == 'off' ~ 'Passing Offense',
#       pass == 0 & side == 'off' ~ 'Rushing Offense',
#       pass == 1 & side == 'def' ~ 'Passing Defense',
#       pass == 0 & side == 'def' ~ 'Rushing Defense'
#     ),
#     bullet = paste0('. ', ordinal(facet_rank), ' in ', game_facet)
#   ) %>% 
#   pull(bullet) %>% 
#   paste(collapse = ' <> ')
# 

