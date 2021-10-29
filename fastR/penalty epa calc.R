source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- load_pbp(2021)

##########

### the goal here is to isolate the EPA impact of individual penalties
### "no play" penalty EPA is taken as is, but other penalties are calculated below
### credit is given to the offense for yards gained up the point where the penalty was enforced
### play_alone_ep_df will look at the play following the penalty and adjust the down/dist/yardline for yards legitimately gained
### this does not handle penalties enforced on or from extra points

play_alone_ep_df <- pbp_df %>% 
  ### remove any "plays" where no action occurred (end of half, TOs, Two-Min Warn)
  filter(timeout == 1 & quarter_end == 0 & desc != 'Two-Minute Warning') %>% 
  ### this group by keeps the lag functions "fenced in" to a only the half
  group_by(game_id, game_half) %>% 
  mutate(
    ### check for change of possession
    pos_change = ifelse(posteam != lead(posteam), 1, 0),
    prev_pos_change = lag(pos_change),
    ### mark all PATs
    is_PAT = ifelse(play_type != 'kickoff' & is.na(down), 1, 0),
    ### set the current first down marker in yardline_100 format
    first_down_marker = yardline_100 - ydstogo,
    ### determine penalty yards from posteam perspective (negative = bad for off)
    net_pen_yds = ifelse(penalty_team == posteam, -penalty_yards, penalty_yards),
    ### change direction of penalty yards if possession just changed
    net_pen_yds = ifelse(pos_change == 1, -net_pen_yds, net_pen_yds),
    ### see penalty yards from previous play
    prev_play_net_pen_yds = lag(net_pen_yds),
    ### prior play_id
    prev_play_id = lag(play_id),
    ### did the prior play (the penalty play) produce a first down
    prev_awarded_first_down = lag(first_down),
    ### if 1 then down was replayed, if 0 then penalty was enforced between downs
    replay_down = ifelse(down == lag(down) | prev_awarded_first_down == 1, 1, 0),
    ### if the penalty on the previous play made this play a first down, return it to the prior down
    # down = ifelse(prev_awarded_first_down == 1, lag(down), down),
    ### set the ball at the new yardline after accounting for penalty yards
    yardline_100 = yardline_100 + prev_play_net_pen_yds,
    ydstogo = case_when(
      # 1st & GL after pos change
      prev_pos_change == 1 & yardline_100 < 10 ~ yardline_100,
      # 1st & GL after getting first down pre-penalty
      prev_awarded_first_down == 1 & yardline_100 < lag(first_down_marker) & yardline_100 < 10 ~ yardline_100,
      # 1st & 10 after pos change
      prev_pos_change == 1 | lag(play_type) == 'kickoff' ~ 10,
      # 1st & 10 after getting first down pre-penalty
      prev_awarded_first_down == 1 & yardline_100 < lag(first_down_marker) ~ 10,
      # gained first due to yards from penalty, so 
      prev_awarded_first_down == 1 ~ yardline_100 - lag(first_down_marker),
      # normal down without any firsts
      T ~ ydstogo + prev_play_net_pen_yds
    ),
    down = case_when(
      # 1st down if after a pos change or kickoff
      prev_pos_change == 1 | lag(play_type) == 'kickoff' ~ 1,
      # 1st down after getting first down pre-penalty
      prev_awarded_first_down == 1 & yardline_100 < lag(first_down_marker) ~ 1,
      # if the penalty on the previous play made this play a first down, return it to the prior down
      prev_awarded_first_down == 1 ~ lag(down) + replay_down,
      # add one to the down if it ultimately was replayed
      T ~ down + replay_down
    )
  ) %>% 
  ### keep only plays that follow a penalty, no plays that follow a touchdown or PAT
  filter(lag(penalty) == 1 & lag(touchdown) != 1 & lag(is_PAT) != 1) %>%
  ungroup %>% 
  ### keep only the fields needed to calculate expected points
  select(game_id, play_id = prev_play_id, half_seconds_remaining, yardline_100, season, home_team, posteam, defteam, roof, down, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining, prev_pos_change) %>%
  calculate_expected_points %>%
  # switch ep value if pos changed
  mutate(ep = ifelse(prev_pos_change == 1, -ep, ep)) %>% 
  select(game_id, play_id, play_ep = ep)

### total net pens

pbp_df %>%
  filter(penalty == 1) %>% 
  left_join(play_alone_ep_df) %>% 
  mutate(
    play_ep = ifelse(play_type == 'no_play', ep, play_ep),
    non_pen_epa = play_ep - ep,
    pen_epa = epa - non_pen_epa
  ) %>% 
  mutate(
    benefit_team = ifelse(posteam == penalty_team, defteam, posteam)
  ) %>% 
  pivot_longer(c(penalty_team, benefit_team), names_to = 'pen_team_type', values_to = 'team') %>% 
  mutate(pen_epa = ifelse(pen_team_type == 'penalty_team', -pen_epa, pen_epa)) %>% 
  filter(pen_team_type == 'penalty_team') %>% 
  group_by(team) %>% 
  summarise(
    n = n(),
    tot_pen_epa = sum(pen_epa, na.rm = T),
    pen_yds = sum(penalty_yards, na.rm = T)
  ) %>% 
  arrange(tot_pen_epa)


#### team logo scatter
games_played_df <- pbp_df %>%
  filter(!is.na(posteam)) %>% 
  group_by(team = posteam) %>%
  summarise(games = n_distinct(game_id))
  
viz_df <- pbp_df %>%
  filter(penalty == 1 & !is.na(down) & !grepl('\\(Punt ', desc, ignore.case = T) & !grepl(' punts ', desc, ignore.case = T) & !grepl('Field Goal', desc, ignore.case = T)) %>% 
  left_join(play_alone_ep_df) %>% 
  mutate(
    play_ep = ifelse(play_type == 'no_play', ep, play_ep),
    non_pen_epa = play_ep - ep,
    pen_epa = epa - non_pen_epa
  ) %>% 
  mutate(
    penalty_team_type = ifelse(posteam == penalty_team, 'offense', 'defense'),
    penalty_team_lost_pen_epa = ifelse(penalty_team_type == 'offense', -pen_epa, pen_epa)
  ) %>% 
  group_by(season, team = penalty_team, penalty_team_type) %>% 
  summarise(
    n = n(),
    tot_pen_epa = sum(penalty_team_lost_pen_epa, na.rm = T),
    pen_yds = sum(penalty_yards, na.rm = T),
    .groups = 'drop'
  ) %>%
  left_join(games_played_df) %>% 
  mutate(tot_pen_epa_gm = tot_pen_epa / games) %>% 
  pivot_wider(c(team, season), names_from = penalty_team_type, values_from = tot_pen_epa_gm) %>% 
  hist_logo_url %>% 
  mutate(grob = grob_img_adj(logo_url, alpha = 0.7))
  
p <- viz_df %>% 
  ggplot(aes(y = offense, x = defense)) +
  geom_grob(aes(y = offense, x = defense, label = grob), vp.width = 0.045) +
  labs(
    title = 'EPA Impact From Penalties, 2021 through Week 7',
    subtitle = 'Special Teams Plays Removed  |  Includes only Accepted Penalties Against Team',
    y = 'EPA Lost\nDue to\nOffensive\nPenalties\nPer Game',
    x = 'EPA Lost Due to Defensive Penalties Per Game'
  ) +
  scale_x_continuous(limits = c(0, 6), expand = expansion(add = 0), breaks = seq(0, 7, 0.5)) +
  scale_y_continuous(limits = c(0, 4), expand = expansion(add = 0), breaks = seq(0, 7, 0.5)) +
  theme_SB +
  theme(
    plot.margin = unit(c(7.5,12,7.5,7.5), 'points')
  )

brand_plot(p, asp = 14/9, save_name = 'pen scatter.png', data_home = 'Data: @nflfastR', fade_borders = 'tr', base_size = 4.2)
  


### look at team game log
pbp_df %>%
  filter(penalty == 1 & !is.na(down) & !grepl('\\(Punt ', desc, ignore.case = T) & !grepl(' punts ', desc, ignore.case = T) & !grepl('Field Goal', desc, ignore.case = T)) %>% 
  left_join(play_alone_ep_df) %>% 
  mutate(
    play_ep = ifelse(play_type == 'no_play', ep, play_ep),
    non_pen_epa = play_ep - ep,
    pen_epa = epa - non_pen_epa
  ) %>% 
  mutate(
    penalty_team_type = ifelse(posteam == penalty_team, 'offense', 'defense'),
    penalty_team_lost_pen_epa = ifelse(penalty_team_type == 'offense', -pen_epa, pen_epa)
  ) %>% 
  filter(penalty_team == 'SF') %>% 
  group_by(game_id, penalty_team, penalty_team_type) %>% 
  summarise(
    n = n(),
    tot_pen_epa = sum(penalty_team_lost_pen_epa, na.rm = T),
    pen_yds = sum(penalty_yards, na.rm = T),
    .groups = 'drop'
  ) %>% 
  pivot_wider(c(game_id, penalty_team), names_from = penalty_team_type, values_from = tot_pen_epa)
