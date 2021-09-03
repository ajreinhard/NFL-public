source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(lubridate)

pbp_df <- nflfastR::load_pbp(2016:2017)
team <- 'CLE'

team_pbp <- pbp_df %>% 
  filter(grepl(team, game_id)) %>%
  mutate(gmt_temp_datetime = as.POSIXct(strptime(paste0(as.Date(game_date), ' ', time_of_day), format = '%Y-%m-%d %T', tz = 'GMT'))) %>% 
  group_by(game_id) %>% 
  mutate(
    team_vegas_wp = ifelse(home_team == team, vegas_home_wp, 1 - vegas_home_wp),
    kickoff_datetime = force_tz(as.POSIXct(strptime(paste0(as.Date(game_date), ' ', start_time), format = '%Y-%m-%d %T', tz = 'GMT')), tz = 'America/New_York'),
    time_diff_temp = gmt_temp_datetime - lag(gmt_temp_datetime),
    #day_chng = ifelse(cumsum(ifelse(time_diff_temp < 0 & !is.na(time_diff_temp), 1, 0)) > 0, 1, 0),
    day_chng = ifelse(cumsum(ifelse(gmt_temp_datetime < kickoff_datetime & !is.na(time_diff_temp), 1, 0)) > 0, 1, 0),
    gmt_temp_datetime = NULL,
    time_diff_temp = NULL,
    actual_date = force_tz(as.POSIXct(strptime(paste0(as.Date(game_date),' 00:00:00'), format = '%Y-%m-%d %T'), tz = 'GMT'), tz = 'America/New_York'),
    actual_datetime = with_tz(as.POSIXct(strptime(paste0(as.Date(game_date) + day_chng, ' ', time_of_day), format = '%Y-%m-%d %T', tz = 'GMT')), tz = 'America/New_York'),
    time_diff_sec = as.numeric(difftime(lead(actual_datetime), actual_datetime, units = 'secs')),
    actual_datetime = as.POSIXct(ifelse(time_diff_sec > 600, NA, actual_datetime), origin = '1970-01-01'),
    actual_time = force_tz(as.POSIXct(as.numeric(difftime(actual_datetime, actual_date, units = 'secs')), origin = '1970-01-01', tz = 'GMT'), tz = 'America/New_York')
  ) %>% 
  ungroup 
  
## game count
game_count_df <- team_pbp %>%
  select(game_id) %>% 
  distinct %>% 
  mutate(game_num = row_number())

## win prob pre-game placed at start of day
start_day <- team_pbp %>% 
  group_by(game_id) %>% 
  filter(row_number() == 1) %>% 
  ungroup %>% 
  select(game_id, actual_time, team_vegas_wp) %>% 
  mutate(actual_time = force_tz(as.POSIXct(60*60*8, origin = '1970-01-01', tz = 'UTC'), tz = 'America/New_York'))

## win prob pre-game placed 90 seconds before first non-NA time
start_game <- team_pbp %>% 
  group_by(game_id) %>% 
  summarise(actual_time = min(actual_time, na.rm = T) - 90) %>% 
  left_join(start_day %>% mutate(actual_time = NULL)) %>% 
  ungroup %>% 
  select(game_id, actual_time, team_vegas_wp)

## win prob post-game placed at end of day
end_day <- team_pbp %>% 
  group_by(game_id) %>% 
  filter(row_number() == max(row_number())) %>% 
  ungroup %>% 
  select(game_id, actual_time, team_vegas_wp) %>% 
  mutate(actual_time = force_tz(as.POSIXct(0, origin = '1970-01-02', tz = 'UTC'), tz = 'America/New_York'))

## win prob post-game placed 90 seconds before last non-NA time
end_game <- team_pbp %>% 
  group_by(game_id) %>% 
  summarise(actual_time = max(actual_time, na.rm = T) + 90) %>% 
  left_join(end_day %>% mutate(actual_time = NULL)) %>% 
  ungroup %>% 
  select(game_id, actual_time, team_vegas_wp)

p <- team_pbp %>% 
  bind_rows(start_day, start_game, end_game, end_day) %>% 
  left_join(game_count_df) %>% 
  ggplot(aes(x = actual_time, ymin = game_num - (1 - team_vegas_wp)/2, ymax = game_num - team_vegas_wp - (1 - team_vegas_wp)/2, group = game_id)) +
  geom_ribbon(fill = NFL_sec[paste0(team)]) +
  geom_image(aes(y = ifelse(game_num == nrow(game_count_df) & game_seconds_remaining == 3600, game_num, NA)), image = 'C:/Users/Owner/Desktop/signiture.png', x = force_tz(as.POSIXct(0, origin = '1970-01-02', tz = 'UTC'), tz = 'America/New_York'), hjust = 1, position = position_nudge(y = 0.3), asp = 16/20, size = 0.08, color = NFL_sec[paste0(team)]) +
  scale_y_reverse(expand = expansion(add = 0), limits = c(32, 0)) +
  scale_x_datetime(expand = expansion(add = 0), limits = c(force_tz(as.POSIXct(60*60*8, origin = '1970-01-01', tz = 'UTC'), tz = 'America/New_York'), force_tz(as.POSIXct(0, origin = '1970-01-02', tz = 'UTC'), tz = 'America/New_York'))) +
  theme_void()

ggsave('graphs/2016-17 browns all win prob.png', p, dpi = 'retina', width = 16, height = 20)



# team_pbp %>% 
#   arrange(-time_diff_sec) %>% 
#   select(game_id, play_id, time_of_day, actual_datetime, actual_date, actual_time, time_diff_sec) 


# team_pbp <- pbp_df %>% 
#   filter(grepl(team, game_id)) %>%
#   mutate(
#     actual_hour = hour(strptime(time_of_day, format = '%H')),
#     actual_day = as.Date(game_date) + ifelse(actual_hour < 6, 1, 0),
#     actual_datetime = with_tz(as.POSIXct(strptime(paste0(actual_day, ' ', time_of_day), format = '%Y-%m-%d %T', tz = 'UTC')), tz = 'America/New_York'),
#     actual_time = as.POSIXct(as.numeric(actual_datetime) %% 86400, origin = '1970-06-01'),
#     team_vegas_wp = ifelse(home_team == team, vegas_home_wp, 1 - vegas_home_wp)
#   )
#   

# team_pbp %>%
#  group_by(game_id) %>%
#  summarise(
#    tm = min(actual_time, na.rm = T),
#    dt = min(actual_date, na.rm = T),
#    dt_tm = min(actual_datetime, na.rm = T),
#    ko = min(kickoff_datetime, na.rm = T)
#  ) %>% 
#   view
# 
# 
# team_pbp$start_time %>% table
# 
# team_pbp %>%
#   filter(game_id == '2016_10_CLE_BAL') %>%
#   select(game_id, play_id, time_of_day, actual_datetime, actual_date, actual_time, team_vegas_wp) %>%
#   view
# 
# 
# team_pbp %>%
#   group_by(game_id) %>% 
#   summarise(time = sum(ifelse(is.na(time_of_day), 1, 0))) %>% 
#   view
