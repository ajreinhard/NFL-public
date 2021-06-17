setwd('C:/Users/rei1740/Desktop/Anthony/nfl')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

games_df <- get_games()

wordmarks_df <- tibble(order = 1:32, team = factor(.tm_div_order, rev(.tm_div_order)), mark = wordmark_url(.tm_div_order)) %>% 
  mutate(img = grob_img_adj(mark))


# slate viz
games_df %>% 
  filter(season == 2021 & game_type == 'REG') %>%
  mutate(game_slate = paste0(weekday, ' ',gametime)) %>% 
  group_by(game_slate) %>% 
  summarise(n = n())

bonus_df <- data.frame(
  team = c('NYJ','ATL','MIA','JAX','DET','CHI','DAL','LV','BUF','NO','CLE','GB','IND','ARI'),
  game_grp = c(rep('Early',6),rep('Late',2),rep('Primetime',2),rep('Late',2),rep('Primetime',2)),
  img = c(rep('other/uk.png',4),rep('other/turkey.png',6),rep('other/christmas.png',4)),
  n = 0
)

p <- games_df %>%
  double_games() %>% 
  separate(gametime, sep = ':', into = c('gamehr', 'gamemin'), remove = F) %>% 
  mutate(
    gamehr = as.numeric(gamehr),
    game_grp = case_when(
      gamehr <= 13 ~ 'Early\nWindow',
      gamehr > 15 & gamehr < 18 ~ 'Late\nWindow',
      gamehr > 18 & weekday == 'Monday' ~ 'Monday\nNight\nFootball',
      gamehr > 18 & weekday == 'Sunday' ~ 'Sunday\nNight\nFootball',
      gamehr > 18 & (weekday == 'Thursday' | weekday == 'Saturday') ~ 'Thursday\nNight\nFootball'
    ),
    team = ifelse(team == 'OAK', 'LV', team)
  ) %>% 
  filter(season == 2021 & game_type == 'REG') %>%
  #filter(is.na(game_grp))
  #filter(team == 'GB') %>% select(week, opp, gametime, weekday)
  group_by(team, game_grp) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  right_join(expand.grid(team = .tm_div_order,  game_grp = c('Early\nWindow', 'Late\nWindow','Thursday\nNight\nFootball','Sunday\nNight\nFootball','Monday\nNight\nFootball'))) %>% 
  mutate(
    n = ifelse(is.na(n), 0, n),
    team = factor(team, rev(.tm_div_order)),
    game_grp = factor(game_grp, c('Early\nWindow', 'Late\nWindow','Thursday\nNight\nFootball','Sunday\nNight\nFootball','Monday\nNight\nFootball'))
  ) %>% 
  group_by(game_grp) %>% 
  mutate(freq = n / max(n)) %>% 
  ungroup %>% 
  ggplot(aes(x = game_grp, y = team, label = n)) +
  geom_grob(data = wordmarks_df, aes(x = -0.3, y = team, label = img), vp.height = 0.02) +
  geom_tile(aes(fill = freq), color = 'darkblue', size = 0.8, show.legend = F) + 
  geom_text(family = font_SB, color = 'darkblue', size = 3) +
  geom_image(data = bonus_df, aes(image = img, x = as.numeric(game_grp) + 0.2), asp = 3/4, size = 0.03) +
  labs(title = 'Games by Time Slot, 2021',
       subtitle = NULL,
       x = NULL,
       y = NULL) +
  scale_fill_gradient2(low = full_alpha_hex(color_SB[5], 0.8), high = full_alpha_hex(color_SB[1], 0.8), mid = 'grey90', midpoint = 0) +
  scale_x_discrete(expand = expansion(add = c(2,0.8)), position = 'top') +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  theme_SB +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank()
  )
  
brand_plot(p, asp = 3/4, save_name = 'game slots.png', data_home = 'Data: nflgamedata.com')

# sched viz
all_poss_wks <- expand.grid(team = .tm_div_order, week = 1:18)

szn_game_df <- games_df %>%
  double_games() %>% 
  separate(gametime, sep = ':', into = c('gamehr', 'gamemin'), remove = F) %>% 
  mutate(
    gamehr = as.numeric(gamehr),
    game_grp = case_when(
      gamehr <= 13 ~ 'Early\nWindow',
      gamehr > 15 & gamehr < 18 ~ 'Late\nWindow',
      gamehr > 18 & weekday == 'Monday' ~ 'Monday\nNight\nFootball',
      gamehr > 18 & weekday == 'Sunday' ~ 'Sunday\nNight\nFootball',
      gamehr > 18 & (weekday == 'Thursday' | weekday == 'Saturday') ~ 'Thursday\nNight\nFootball'
    ),
    team = ifelse(team == 'OAK', 'LV', team)
  ) %>% 
  filter(season == 2021 & game_type == 'REG') %>% 
  right_join(all_poss_wks) %>% 
  mutate(
    wk_lab = ifelse(is.na(game_id), 'BYE', ''),
    team = factor(team, rev(.tm_div_order)),
    game_grp = factor(game_grp, c('Early\nWindow', 'Late\nWindow','Thursday\nNight\nFootball','Sunday\nNight\nFootball','Monday\nNight\nFootball'))
  )
szn_game_df

triangle_corner_pt <- szn_game_df %>% mutate(x = week + 0.5, y = as.numeric(team) + 0.5)
triangle_right_pt <- szn_game_df %>% mutate(x = week, y = as.numeric(team) + 0.5)
triangle_bottom_pt <- szn_game_df %>% mutate(x = week + 0.5, y = as.numeric(team))
triangle_df <- rbind(triangle_corner_pt, triangle_right_pt, triangle_bottom_pt) %>% filter(grepl('Night',game_grp))

p <- szn_game_df %>%
  ggplot(aes(x = week, y = team)) +
  geom_text(aes(label = wk_lab), color = 'grey70', size = 1.8, angle = 45, show.legend = F, na.rm = T) +
  geom_tile(aes(fill = location), color = 'grey55', size = 0.5, show.legend = F, na.rm = T) +
  geom_image(aes(image = ESPN_logo_url(opp)), size = 0.03, asp = 0.85) +
  geom_polygon(data = triangle_df, aes(x = x, y = y, group = interaction(team, week)), fill = color_SB[5], color = 'grey55', size = 0.5, show.legend = F) +
  geom_text(aes(label = ifelse(grepl('Night',game_grp), substr(weekday,1,1), NA)), color = 'darkblue', size = 1, nudge_x = 0.35, nudge_y = 0.35) +
  geom_vline(xintercept = c(6.5,12.5), color = 'darkblue', size = 0.6) +
  geom_hline(yintercept = seq(4,28,4) + 0.5, color = 'darkblue', size = 0.6) +
  annotation_custom(make_gradient(deg = 180), ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=0) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=18.5, xmax=Inf) +
  annotation_custom(make_gradient(deg = 270), ymin=32.5, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 90), ymin=-Inf, ymax=0.5, xmin=-Inf, xmax=Inf) +
  geom_grob(data = wordmarks_df, aes(x = -2.2, y = team, label = img), vp.height = 0.02) +
  #geom_text(aes(label = opp), size = 1) +
  labs(title = 'NFL Regular Season Schedule, 2021',
       subtitle = NULL,
       x = '                                            Week',
       y = NULL) +
  scale_fill_manual(values = c('Home' = full_alpha_hex(color_SB[1], 0.3), 'Away' = NA, 'Nuetral' = NA)) +
  scale_x_continuous(breaks = 1:18, expand = expansion(add = c(3,0.4))) +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  theme_SB +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.text.x = element_text(margin = margin(t = 0, unit = 'pt'))
  )

brand_plot(p, asp = 3/4, save_name = 'sched viz.png', data_home = 'Data: nflgamedata.com')



# visualize the spread
library(ggtext)

lines_df <- read_csv('https://raw.githubusercontent.com/nflverse/nfldata/master/data/initial_lines.csv')

games_df <- get_games() %>%
  double_games() %>% 
  filter(season == 2021 & game_type == 'REG')

graph_df <- lines_df %>% 
  filter(type == 'SPREAD') %>% 
  rename(game_id = about, team = side) %>% 
  left_join(games_df) %>% 
  right_join(expand.grid(team = .tm_div_order_alt, week = 1:18)) %>% 
  group_by(season, team) %>% 
  arrange(week) %>% 
  mutate(
    gm_num = row_number(),
    gm_row = (week-1) %% 3 + 1,
    gm_col = floor((week-1) / 3) + 1,
    team = factor(team, .tm_div_order_alt)
  ) %>% 
  ungroup %>% 
  separate(gametime, sep = ':', into = c('gm_hr', 'gm_mn')) %>% 
  mutate(
    gm_hr = as.numeric(gm_hr),
    game_desc = case_when(
      gm_hr == 9 ~ 'UK',
      gameday == '2021-11-25' ~ 'THXG',
      gm_hr == 20 & weekday == 'Sunday' ~ 'SNF',
      gm_hr == 20 & weekday == 'Monday' ~ 'MNF',
      gm_hr == 20 & weekday == 'Thursday' ~ 'TNF',
      gameday == '2021-12-25' ~ 'XMAS',
      T ~ ''
    )
  )

grob_df <- graph_df %>% 
  filter(!is.na(season)) %>% 
  rename(true_tm = team, team = opp) %>% 
  hist_logo_url %>% 
  rename(opp = team, team = true_tm) %>% 
  mutate(img = grob_img_adj(logo_url, bw = T)) 

p <- graph_df %>% 
  ggplot(aes(x = gm_row, y = gm_col)) +
  facet_wrap(~team, nrow = 4) +
  geom_tile(aes(fill = ifelse(location == 'Home' | is.na(location),'grey95','grey89')), color = 'grey65', show.legend = F, size = 0.5, na.rm = T) +
  geom_grob(data = grob_df, aes(x = gm_row + 0.33, y = gm_col + 0.05, label = img), vp.width = 0.095) +
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.2), yend = gm_col + 0.2, x = gm_row - 0.4, xend = gm_row + 0.15), color = 'grey60', size = 0.2) +
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.12), yend = gm_col + 0.28, x = gm_row + ((0.55 / 28) * 7) - 0.125, xend = gm_row + ((0.55 / 28) * 7) - 0.125), color = 'grey60', size = 0.1) +
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.12), yend = gm_col + 0.28, x = gm_row + ((0.55 / 28) * 14) - 0.125, xend = gm_row + ((0.55 / 28) * 14) - 0.125), color = 'grey60', size = 0.1) +
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.12), yend = gm_col + 0.28, x = gm_row + ((0.55 / 28) * -7) - 0.125, xend = gm_row + ((0.55 / 28) * -7) - 0.125), color = 'grey60', size = 0.1) +
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.12), yend = gm_col + 0.28, x = gm_row + ((0.55 / 28) * -14) - 0.125, xend = gm_row + ((0.55 / 28) * -14) - 0.125), color = 'grey60', size = 0.1) +
  geom_segment(aes(y = gm_col + 0.2, yend = gm_col + 0.2, x = gm_row - 0.125, xend = gm_row + ((0.55 / 28) * line) - 0.125, color = ifelse(line > 0, 'darkblue', color_SB[1])), size = 0.4) + 
  geom_segment(aes(y = ifelse(is.na(season), NA, gm_col + 0.08), yend = gm_col + 0.32, x = gm_row - 0.125, xend = gm_row - 0.125), color = 'grey60', size = 0.15) +
  geom_text(aes(x = gm_row - 0.4, y = gm_col - 0.35), label = 'Wk', color = 'grey60', family = font_SB, size = 0.8) +
  geom_text(aes(x = gm_row - 0.4, y = gm_col - 0.1, label = week), color = 'grey60', family = font_SB, size = 1) +
  geom_text(aes(x = gm_row - 0.125, y = gm_col - 0.15, label = ifelse(line > 0, paste0('+',line), ifelse(line == 0, 'PK', line)), color = ifelse(line > 0, 'darkblue', ifelse(line == 0, 'grey40', color_SB[1]))), family = font_SB, size = 1.2) +
  geom_text(aes(x = gm_row + 0.1, y = gm_col - 0.15, label = ifelse(location == 'Away', '@', 'vs')), family = font_SB, size = 0.8, color = 'grey60') +
  geom_text(aes(x = gm_row + 0.47, y = gm_col - 0.37, label = game_desc), family = font_SB, size = 0.8, color = 'grey60', hjust = 1) +
  geom_text(aes(x = ifelse(is.na(season), gm_row, NA)), label = 'BYE', family = font_SB, size = 2, color = 'grey60') +
  scale_y_reverse(expand = expansion(mult = 0.005)) + 
  scale_x_continuous(expand = expansion(mult = 0.005)) + 
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = '2021 Opening Lines (via Westgate SuperBook)',
       subtitle = NULL,
       y = NULL,
       x = NULL) +
  theme_SB +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing.x = unit(0.4, 'lines'),
    panel.spacing.y = unit(0.1, 'lines')
    #plot.subtitle = element_markdown(family = font_SB)
  )

brand_plot(p, asp = 16/9, save_name = '2021 lines.png', data_home = 'Data: Westgate SuperBook', tm_wordmarks = T)

