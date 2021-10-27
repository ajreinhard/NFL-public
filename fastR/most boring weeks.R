source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- load_pbp(1999:2021)

garb_df <- pbp_df %>% 
  filter(qtr <= 4 & !is.na(vegas_wp) & season_type == 'REG') %>% 
  group_by(game_id) %>% 
  fill(game_seconds_remaining) %>% 
  mutate(
    sec_elap = -diff(c(game_seconds_remaining,0)),
    fav_wp = ifelse(vegas_wp < .5, 1 - vegas_wp, vegas_wp),
    above_thresh = ifelse(fav_wp > 0.9, 'Above Thresh', 'Below Thresh')
  ) %>% 
  group_by(season, week, above_thresh) %>% 
  summarise(mins = sum(sec_elap, na.rm = T) / 60) %>% 
  group_by(season, week) %>%
  mutate(tot_week = sum(mins)) %>% 
  ungroup %>% 
  mutate(pct_thresh = mins / tot_week) %>% 
  filter(above_thresh == 'Below Thresh') %>% 
  arrange(mins)

big_play_df <- pbp_df %>% 
  filter(season_type == 'REG') %>% 
  group_by(season, week) %>% 
  summarise(
    big_plays = sum(ifelse(abs(epa) > 2, 1, 0), na.rm = T),
    big_play_rt = mean(ifelse(abs(epa) > 2, 1, 0), na.rm = T),
    .groups = 'drop'
  ) %>% 
  arrange(big_plays)

desc = 'The 13 games played in Week 7 of 2021 had\njust 351 plays where either team gained more than\ntwo expected points and had just over 480 minutes\nof game action where both teams had less than a\n90% chance to win.'

p <- big_play_df %>% 
  left_join(garb_df) %>% 
  arrange(season) %>% 
  ggplot(aes(x = big_plays, y = mins)) +
  geom_hline(aes(yintercept = mean(mins)), color = color_SB[2], size = 0.6, linetype = '52', alpha = 0.4) +
  geom_vline(aes(xintercept = mean(big_plays)), color = color_SB[2], size = 0.6, linetype = '52', alpha = 0.4) +
  geom_point(aes(color = ifelse(season == 2021, 2021, 'other'), alpha = ifelse(season == 2021, 2021, 'other')), fill = 'skyblue', shape = 21, size = 1.7, show.legend = F) +
  geom_point(aes(x = ifelse(season == 2021 & week == 7, big_plays, NA)), color = 'black', fill = color_SB[1], shape = 21, size = 1.7, show.legend = F, na.rm = T) +
  geom_curve(x = 420, y = 425, xend = 355, yend = 480.4, color = 'darkblue', curvature = 0.3, size = 0.5, arrow = arrow(length = unit(2, "mm"))) +
  geom_shadowtext(aes(x = ifelse(season == 2021 & week == 7, 440, NA)), y = 400, label = desc, color = 'darkblue', family = font_SB, bg.color = 'white', bg.r = 0.3, size = 1.8, na.rm = T) +
  geom_shadowtext(aes(x = ifelse(season == 2021 & week == 7, 300, NA)), y = 460, label = 'Boring', color =  color_SB[2], family = font_SB, bg.color = 'white', bg.r = 0.3, size = 1.8, fontface = 'italic', na.rm = T) +
  geom_shadowtext(aes(x = ifelse(season == 2021 & week == 7, 450, NA)), y = 460, label = 'Big Plays\nNot Close', color = color_SB[2], family = font_SB, bg.color = 'white', bg.r = 0.3, size = 1.8, fontface = 'italic', na.rm = T) +
  geom_shadowtext(aes(x = ifelse(season == 2021 & week == 7, 300, NA)), y = 800, label = 'Few Big Plays\nClose Games', color = color_SB[2], family = font_SB, bg.color = 'white', bg.r = 0.3, size = 1.8, fontface = 'italic', na.rm = T) +
  geom_shadowtext(aes(x = ifelse(season == 2021 & week == 7, 450, NA)), y = 800, label = 'Exciting', color = color_SB[2], family = font_SB, bg.color = 'white', bg.r = 0.3, size = 1.8, fontface = 'italic', na.rm = T) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(360, 900), breaks = seq(360, 900, 60)) +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(250, 500)) +
  scale_color_manual(values = c('2021' = 'black', 'other' = 'grey50')) +
  scale_alpha_manual(values = c('2021' = 1, 'other' = 0.4)) +
  labs(title = 'Football was pretty boring this week',
       subtitle = 'Big Plays vs Competitiveness in Each Week, 1999-2021 Wk 7  |  2021 Weeks Outlined',
       y = 'Regulation\nMins w/\nSpread-Adj\nWin Prob\nBetween\n10% & 90%',
       x = '# of Plays with Less than -2 EPA or More than +2 EPA') +
  theme_SB +
  theme(
    plot.margin = margin(c(7.5,12,7.5,7.5), unit = 'pt'),
    plot.subtitle = element_text(size = 7)
  )

brand_plot(p, save_name = 'boring week.png', data_home = 'Data: @nflfastR', fade_borders = 'tr')