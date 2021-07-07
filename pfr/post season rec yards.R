complete_box <- readRDS('pfr/data/boxscores.rds')


complete_box %>% 
  filter(game_type != 'REG' & season >= 1966) %>% 
  group_by(player_id, player) %>% 
  summarise(
    n = n(),
    rec = sum(rec),
    rec_yds = sum(rec_yds),
    rec_TDs = sum(rec_td),
    rec_gm = rec / n,
    rec_yds_gm = rec_yds / n
  ) %>% 
  arrange(-rec_gm) %>% 
  ungroup %>% 
  filter(n >= 5) %>% 
  mutate(rank = row_number())
  

complete_box %>% 
  filter(season >= 1966) %>% 
  group_by(player_id, player, season_type = ifelse(game_type == 'REG','REG','POST')) %>% 
  summarise(
    n = n(),
    rec = sum(rec, na.rm = T),
    tot_rec_yds = sum(rec_yds),
    rec_yds_gm = sum(rec_yds) / n
  ) %>% 
  ungroup %>% 
  pivot_wider(values_from = c(n, rec, tot_rec_yds, rec_yds_gm), names_from = season_type) %>% 
  #filter(n_POST >= 5 & (rec_POST + rec_REG) >= 100) %>% 
  #arrange(-(rec_yds_gm_POST - rec_yds_gm_REG)) %>% view
  ggplot(aes(x = tot_rec_yds_REG, y = tot_rec_yds_POST, label = player_id)) +
  geom_point() +
  geom_text_repel(max.overlaps = 1)

?geom_text_repel
