setwd('C:/Users/Owner/Documents/GitHub/NFL-public')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

roster_df <- fast_scraper_roster(1999:2020)
future::plan("multisession")
pbp_df <- load_pbp(1999:2010) %>% 
  filter(!is.na(receiver_id)) %>% 
  select(season, receiver_id, complete_pass, yards_gained, epa)

rec_df <- do.call(rbind, lapply(1999:2020, function(yr) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',yr,'.rds'))) %>% 
    filter(!is.na(receiver_id)) %>% 
    select(season, receiver_id, complete_pass, yards_gained, epa)
}))

p <- rec_df %>%
  filter(season >= 2004) %>% 
  rename(gsis_id = receiver_id) %>% 
  left_join(roster_df) %>% 
  team2fran %>% 
  mutate(
    jersey_grp = floor(jersey_number/10) * 10,
    jersey_grp = as.factor(paste0(jersey_grp,'s')),
    team = factor(team, .tm_div_order_alt)
  ) %>% 
  #filter(position == 'WR' & !is.na(jersey_grp)) %>% 
  group_by(season, team, jersey_grp) %>% 
  summarise(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup %>% 
  filter(jersey_grp %in% c('10s','80s')) %>% 
  pivot_wider(c(season, team), names_from = jersey_grp, values_from = share, values_fill = list(share = 0)) %>% 
  ggplot(aes(x = season)) +
  facet_wrap(.~team, nrow = 4, scales = 'free') +
  annotation_custom(make_gradient(deg = 270), ymin=0.9, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2018.5, xmax=Inf) +
  geom_line(aes(color = '80s', y = `80s`)) +
  geom_point(aes(color = '80s', y = `80s`), size = 0.7) +
  geom_line(aes(color = '10s', y = `10s`)) +
  geom_point(aes(color = '10s', y = `10s`), size = 0.7) +
  scale_y_continuous(limits = c(0,1), labels = percent, breaks = seq(0,1,.2), expand = expansion(add = 0)) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  scale_color_manual(values = color_SB) +
  labs(title = 'Target Share by Jersey Number, 2004-2020',
       subtitle = NULL,
       y = NULL,
       x = NULL,
       color = 'Jersey Number') +
  theme_SB + 
  theme(
    axis.text = element_text(size = 4),
    panel.spacing.x = unit(0.3, 'lines'),
    panel.spacing.y = unit(0.1, 'lines'),
    legend.position = c(0.82,1.127),
    legend.key.height = unit(0.5, 'lines'),
    legend.key.width = unit(0.2, 'lines'),
    legend.direction = 'horizontal',
    legend.margin = margin(t = 0.3, b = 0.3, r = 0.3, l = 0.3, unit='lines'),
    axis.line = element_line(color = 'darkblue', size = 0.5),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5),
    plot.margin = unit(c(7.5,9,11,7.5), 'point')
  )

brand_plot(p, save_name = 'graphs/jersey targ share team.png', data_home = 'Data: @nflfastR', asp = 16/9, tm_wordmarks = T, base_size = 4)
