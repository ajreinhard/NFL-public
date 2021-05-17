setwd('C:/Users/rei1740/Desktop/Anthony/nfl')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))) %>% decode_player_ids()
roster_df <- fast_scraper_roster(2020)

player_df <- pbp_df %>% 
  filter(!is.na(air_yards) & !is.na(receiver_id) & air_yards <= 70 & air_yards >= -15) %>% 
  group_by(season, posteam, receiver_id) %>% 
  summarise(n = n(), mean_air_yards = mean(air_yards)) %>% 
  arrange(-n) %>% 
  mutate(team_targ_rank = row_number()) %>% 
  filter(team_targ_rank <= 4) %>%
  arrange(-mean_air_yards) %>% 
  mutate(team_air_rank = row_number()) %>% 
  left_join(roster_df, by = c('season', 'receiver_id' = 'gsis_id')) %>% 
  mutate(jersey_number = NULL) %>% 
  ungroup

tm_grob_df <- data.frame(posteam = .tm_div_order, team_air_rank = 2, full_name = NA, headshot_url = ESPN_logo_url(.tm_div_order), air_yards = 20, vp.height = 1, alpha = 0.4)

grob_df <- player_df %>% 
  select(posteam, team_air_rank, full_name, headshot_url) %>% 
  mutate(air_yards = 40, vp.height = 0.4, alpha = 0, team_air_rank = team_air_rank) %>% 
  mutate(posteam = factor(posteam, .tm_div_order))

grob_df$grob <- sapply(1:nrow(grob_df), function(x) grob_img_adj(grob_df$headshot_url[x], alpha = grob_df$alpha[x]))

my_week <- 14

p <- pbp_df %>% 
  filter(!is.na(air_yards) & !is.na(receiver_id) & air_yards <= 70 & air_yards >= -15) %>% 
  right_join(player_df) %>%
  mutate(posteam = factor(posteam, .tm_div_order)) %>% 
  ggplot(aes(group = team_air_rank, x = air_yards, y = team_air_rank)) +
  facet_wrap(.~posteam, nrow = 8, scales = 'free_x') +
  geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, asp = 1.375) +
  geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, color = 'white', alpha = 0.8, asp = 1.375) +
  geom_vline(xintercept = seq(0,40,10), color = 'grey85', size = 0.3) +
  geom_grob(data = grob_df, aes(x = air_yards, y = team_air_rank - 0.4, label = grob, vp.height = vp.height)) +
  geom_hline(data = grob_df, aes(yintercept = ifelse(is.na(full_name), NA, team_air_rank), color = posteam), size = 0.6, show.legend = F) +
  geom_density_ridges(aes(color = posteam, fill = posteam), scale = 1.2, bandwidth = 3, panel_scaling = F, show.legend = F, size = 0.4) +
  geom_shadowtext(data = grob_df, aes(label = full_name, x = air_yards - 3, y = team_air_rank - 0.6), color = 'darkblue', bg.color = 'grey95', family = font_SB, bg.r = 0.2, hjust = 1, size = 1.5) +
  scale_y_reverse(expand = expansion(add = c(0.2,0.2))) +
  scale_x_continuous(limits = c(-5,45), expand = expansion(mult = 0)) +
  scale_color_manual(values = NFL_sec) +
  scale_fill_manual(values = NFL_pri) +
  labs(title = '2020 Targeted Air Yards Distribution',
       subtitle = paste0('Four most targeted players on each team through Week ', my_week),
       x = NULL,
       y = NULL) +
  theme_SB +
  theme(
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 4),
    axis.line.x = element_line(color = 'darkblue', size = 0.5),
    axis.line.y = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.ticks.length = unit(0.1, 'lines'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5)
  )


brand_plot(p, asp = 9/16, save_name = 'air yards team panel 2020.png', data_home = 'Data: @nflfastR')


