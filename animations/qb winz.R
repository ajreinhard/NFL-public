setwd('C:/Users/Owner/Documents/GitHub/NFL-public')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(ggstance)

elo_df <- get_538elo() %>% double_games()

all_weeks <- elo_df %>% 
  filter(season >= 1966) %>% 
  select(season, week) %>% 
  distinct %>% 
  mutate(
    week_order = row_number(),
    week_name = paste0('As of Week ',week, ', ', season)
  )


qb_wins <- elo_df %>% 
  select(season, week, team_qb, result, game_type) %>% 
  left_join(all_weeks) %>% 
  mutate(
    week_order = ifelse(is.na(week_order), 0, week_order)
  ) %>% 
  full_join(all_weeks %>% select(week_order), by = character(), suffix = c('_past','')) %>%
  filter(week_order_past <= week_order & team_qb != '') %>% 
  group_by(week_order, team_qb) %>% 
  summarise(
    starts = n(),
    wins = sum(ifelse(result > 0, 1, 0)),
    sb_wins = sum(ifelse(game_type == 'SB' & result > 0, 1, 0)),
    last_start_week = max(week_order_past),
    .groups = 'drop'
  ) %>% 
  group_by(team_qb) %>% 
  filter(max(last_start_week) > 0) %>% 
  ungroup %>% 
  arrange(-wins) %>% 
  group_by(week_order) %>% 
  mutate(
    rank = row_number(),
    wins_leader = max(wins)
  ) %>% 
  ungroup %>% 
  arrange(week_order, rank)
  
every_qb <- elo_df %>% filter(team_qb != '') %>% pull(team_qb) %>% unique %>% tibble %>% rename(team_qb = 1)

weekly_qb_team <- elo_df %>%
  filter(team_qb != '') %>% 
  select(season, week, team, team_qb) %>% 
  right_join(
    full_join(all_weeks, every_qb, by = character())
  ) %>%
  group_by(season, team_qb) %>% 
  fill(team, .direction = 'downup') %>%
  ungroup %>% 
  arrange(week_order)

### QBs that missed a year
qb_wins %>% 
  left_join(weekly_qb_team) %>% 
  group_by(team_qb) %>% 
  mutate(retire_season = max(ifelse(!is.na(team), season + 1, NA), na.rm = T)) %>% 
  #filter(team_qb == 'Peyton Manning')
  filter(rank <= 20 & season < retire_season & is.na(team)) %>% 
  select(season, team_qb) %>% 
  distinct
  
weekly_qb_team <- weekly_qb_team %>% 
  mutate(
    team = case_when(
      season == 1966 & team_qb == 'Jim Ninowski' ~ 'CLE',
      season == 1967 & team_qb == 'George Blanda' ~ 'OAK',
      season == 1967 & team_qb == 'Charley Johnson' ~ 'SLC',
      season == 1967 & team_qb == 'Earl Morrall' ~ 'NYG',
      season == 1968 & team_qb == 'Johnny Unitas' ~ 'BLC',
      season == 1968 & team_qb == 'Jack Kemp' ~ 'BUF',
      season == 1982 & team_qb == 'Jim Hart' ~ 'SLC',
      season == 1987 & team_qb == 'Joe Ferguson' ~ 'DET',
      season %in% c(1987, 1988) & team_qb == 'Ron Jaworski' ~ 'MIA',
      season %in% c(1991, 1992) & team_qb == 'Joe Montana' ~ 'SF',
      season == 1999 & team_qb == 'Warren Moon' ~ 'KC',
      season == 2011 & team_qb == 'Peyton Manning' ~ 'IND',
      T ~ team
    )
  )

retire_logo <- elo_df %>% 
  team2fran %>% 
  filter(team_qb != '') %>% 
  group_by(team_qb, team) %>% 
  summarise(
    n = n(),
    season = max(season)
  ) %>% 
  arrange(-n, -season) %>% 
  filter(row_number() == 1) %>% 
  ungroup %>% 
  hist_logo_url %>% 
  select(team_qb, ret_logo = logo_url)

lombardi_img <- qb_wins %>% 
  filter(rank <= 20) %>% 
  full_join(tibble(sb_cnt = 1:max(qb_wins$sb_wins)), by = character()) %>% 
  filter(sb_wins >= sb_cnt) %>% 
  select(week_order, team_qb, rank, wins, wins_leader, sb_cnt) %>% 
  mutate(x_loc = wins - sb_cnt * (wins_leader * 0.015) + wins_leader * 0.005)

viz_df <- qb_wins %>% 
  left_join(weekly_qb_team) %>% 
  group_by(team_qb) %>% 
  mutate(retire_season = max(ifelse(!is.na(team), season + 1, NA), na.rm = T)) %>% 
  ungroup %>% 
  filter(rank <= 20 & week_order > 1039) %>% 
  team2fran %>% 
  hist_color_map %>% 
  hist_logo_url %>% 
  left_join(retire_logo) %>% 
  mutate(
    status = ifelse(season >= retire_season, 'Retired', 'Active'),
    color_key = ifelse(status == 'Retired', 'RET', color_key),
    logo_url = ifelse(status == 'Retired', ret_logo, logo_url),
    logo_size = ifelse(status == 'Retired', 0.02, 0.03)
  )

viz_df$grob <- sapply(1:nrow(viz_df), function(x) grob_img_adj(viz_df$logo_url[x], bw = ifelse(viz_df$status[x] == 'Retired', T, F)))
str(viz_df)  

#grob_img_adj(logo_url, bw = ifelse(team == 'RET', T, F))

my_ani <- ggplot(data = viz_df, aes(x = wins, y = rank, group = team_qb)) +
  geom_barh(aes(color = color_key, fill = color_key), stat = 'identity', show.legend = F, size = 2, width = 0.8) +
  geom_text(aes(label = paste0(team_qb, ' (',wins,')'), x = wins - wins_leader * 0.005 - sb_wins * (wins_leader * 0.015), color = status), hjust = 1, size = 7, family = 'Meiryo', show.legend = F) +
  #geom_image(aes(image = logo_url, x = wins + wins_leader * 0.02), asp = 2.25, size = 0.03) +
  geom_grob(data = viz_df, aes(x = wins + wins_leader * 0.02, y = rank, label = grob, vp.width = logo_size)) +
  geom_image(data = lombardi_img %>% filter(week_order > 1039), aes(image = 'C:/Users/Owner/Documents/GitHub/NFL/NFL/lomb.png', x = x_loc), asp = 2.25, size = 0.015) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0,300,25), labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c(NFL_pri, 'RET' = 'grey70')) +
  scale_color_manual(values = c(NFL_sec, 'RET' = 'grey20', 'Active' = 'white', 'Retired' = 'grey20', 'Hall' = 'gold4')) +
  labs(title = 'Wins as a Starting QB by Super Bowl Era Players',
       subtitle = '{all_weeks$week_name[as.numeric(previous_state)]}  |  min. 1 QB Start in Super Bowl Era (1966-Present)',
       x = NULL,
       y = 'QB Winz') +
  vid_theme_SB + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color='grey70', size = 1.2),
    axis.line = element_line(color = 'darkblue', size = 1.5)
  ) +
  transition_states(week_order, transition_length = 5, state_length = 1, wrap = F) +
  view_follow(fixed_y = T) +
  enter_fly(y_loc = -21) +
  exit_fly(y_loc = -21) +
  ease_aes('linear')

#data_home = 'Data: FiveThirtyEight'
anim_save(filename = 'graphs/qb winz short.mp4', animation = animate(my_ani, nframes = 200, fps = 40, height = 900, width = 1600, renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/qb winz short.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/qb winz short working.mp4"')


warnings()

elo_df %>%
  filter(team_qb != '') %>%
  group_by(team_qb) %>% 
  summarise(
    n = n(),
    first_yr = min(season),
    retire_yr = max(season) + 1,
  ) %>% 
  arrange(-n) %>% 
  filter(first_yr >= 1966)



qb_wins %>% 
  filter(rank <= 20 & week_order == 1) %>% 
  left_join(weekly_qb_team)

##############
qb_wins %>% 
  filter(week_order_stop == 1093) %>% 
  arrange(-n) %>% 
  slice(1:20) %>% 
  view


qb_wins %>% 
  filter(team_qb == 'John Hadl') %>% 
  arrange(week_order_stop) %>% 
  view

all_qbs <- qb_wins %>% 
  filter(rank <= 20) %>% 
  arrange(week_order_stop, rank) %>% 
  pull(team_qb)


### need to matchup with teams
### retirement teams?
### hof induction years

elo_df %>% 
  filter(team == 'CLE' & season >= 2006) %>% 
  group_by(team_qb) %>% 
  summarise(
    tot_starts = n(),
    last_start = max(gameday),
    first_start = min(gameday),
    .groups = 'drop'
  ) %>% 
  arrange(first_start) %>% 
  pull(team_qb) %>% 
  paste(collapse = ', ')
  
  
### qbs start on multiple teams
elo_df %>%
  filter(team_qb != '') %>%
  group_by(season, team_qb, team) %>% 
  summarise(starts = n()) %>% 
  mutate(teams = n()) %>% 
  filter(teams > 1 & team_qb %in% all_qbs) %>% 
  view
  


elo_df %>%
  group_by(team) %>% 
  summarise(starts = n())
  
  

all_qbs <- qb_wins %>% 
  filter(rank <= 20)



#### franchises to consider color changes
# TEN/HOU
# STL/LAR
# SD
# SEA
# TB
# PHI

# IND
# CLE
# DET
# MIA
# NE
# MIN
# NO
# NYJ





lombardi_img %>%
  filter(team_qb == 'Tom Brady' & week_order > 1088)


