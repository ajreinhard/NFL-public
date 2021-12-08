setwd('C:/Users/Owner/Documents/GitHub/NFL/NFL')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

yr <- 2021

### get data and dakota model
pbp_df <- nflfastR:::load_pbp(yr)
load(url('https://raw.githubusercontent.com/guga31bb/metrics/master/dakota_model.rda'))
roster_df <- fast_scraper_roster(yr)

library(lubridate)

### create df that gets stats at the end of every hour
hourly_stats <- pbp_df %>% 
  fill(time_of_day) %>% 
  filter(!is.na(id) & !is.na(down) & play == 1) %>% 
  mutate(
    actual_hour = hour(strptime(time_of_day, format = '%H')),
    actual_day = as.Date(game_date) + ifelse(actual_hour < 6, 1, 0),
    actual_datetime = strptime(paste0(actual_day, ' ', time_of_day), format = '%Y-%m-%d %T', tz = 'GMT'),
    actual_datetime_hour = as.POSIXct(ceiling(as.numeric(actual_datetime)/3600) * 3600, origin = '1970-01-01')
  ) %>% 
  group_by(game_date, actual_datetime_hour, gsis_id = id) %>% 
  summarise(
    plays = n(),
    epa_total = sum(ifelse(qb_epa <= -4.5, -4.5, qb_epa), na.rm = T),
    exp_comp = sum(cp, na.rm = T),
    act_comp = sum(ifelse(is.na(cp), NA, complete_pass), na.rm = T),
    pass_att = sum(ifelse(is.na(cp), NA, complete_pass + incomplete_pass + interception), na.rm = T),
    .groups = 'drop'
  ) %>%  
  left_join(roster_df) %>% 
  filter(position == 'QB') %>% 
  arrange(actual_datetime_hour) %>% 
  mutate_at(c('plays','epa_total','act_comp', 'exp_comp', 'pass_att'), function(x) ifelse(is.na(x), 0, x)) %>% 
  group_by(gsis_id) %>% 
  mutate(
    total_plays = cumsum(plays),
    epa_per_play = cumsum(epa_total) / cumsum(plays),
    cpoe = ((cumsum(act_comp) / cumsum(pass_att)) - (cumsum(exp_comp) / cumsum(pass_att))) * 100,
    active = 1
  ) %>% 
  ungroup %>% 
  mutate(dakota = mgcv::predict.gam(dakota_model, .)) %>% 
  select(actual_datetime_hour, game_date, gsis_id, team, last_name, total_plays, epa_per_play, cpoe, dakota, active)

### get every single hour and week that happen in the df
all_hours <- hourly_stats %>% select(actual_datetime_hour, game_date) %>% distinct
game_week <- pbp_df %>% select(game_date, week) %>% distinct

### pad each gameday with an extra hour (games start at like 1:15pm so show 1pm)
begin_day_hour <- hourly_stats %>% 
  mutate(actual_date = as.Date(actual_datetime_hour, tz = 'America/New_York')) %>% 
  group_by(game_date) %>% 
  summarise(actual_datetime_hour = min(actual_datetime_hour) - 3600) %>% 
  select(actual_datetime_hour, game_date)

### pad each week with on extra hour
end_week_extra_hour <- hourly_stats %>% 
  left_join(game_week) %>% 
  group_by(week) %>% 
  summarise(
    actual_datetime_hour = max(actual_datetime_hour) + 3600,
    game_date = max(game_date)
  ) %>% 
  select(actual_datetime_hour, game_date)

### pad start of video with five hours
season_begin <- hourly_stats %>% 
  summarise(
    actual_datetime_hour = min(actual_datetime_hour),
    game_date = min(game_date)
  ) %>% 
  full_join(tibble(minus_hours = (2:6) * 3600), by = character()) %>% 
  mutate(
    actual_datetime_hour = actual_datetime_hour - minus_hours,
    minus_hours = NULL
  )

### pad end of video with eight hours
season_end <- hourly_stats %>% 
  summarise(
    actual_datetime_hour = max(actual_datetime_hour),
    game_date = max(game_date)
  ) %>% 
  full_join(tibble(plus_hours = (2:9) * 3600), by = character()) %>% 
  mutate(
    actual_datetime_hour = actual_datetime_hour + plus_hours,
    plus_hours = NULL
  )

### get all the hours together now
full_hours <- rbind(all_hours, begin_day_hour, end_week_extra_hour, season_begin, season_end)
full_qb <- hourly_stats %>% select(gsis_id) %>% distinct

### describe each of the hours for the subtitle
hours_detail <- full_hours %>%
  left_join(game_week) %>% 
  arrange(actual_datetime_hour) %>% 
  mutate(
    date_time_index = as.numeric(as.factor(actual_datetime_hour)),
    play_min = ifelse(week >= 16, 300, (week - 1) * 20),
    play_min = ifelse(play_min == 0, 1, play_min),
    time_desc = trimws(format(actual_datetime_hour, '%l%p')),
    weekday = format(actual_datetime_hour, '%A'),
    month_day = paste0(as.numeric(format(actual_datetime_hour, '%m')),format(actual_datetime_hour, '/%d/%Y')),
    week_name = 
      case_when(
        week == 18 ~ 'Wild Card Week',
        week == 19 ~ 'Divisional Week',
        week == 20 ~ 'Conference Championship Week',
        week == 21 ~ 'Super Bowl Week',
        T ~ paste0('Week ',week)
      ),
    full_desc = paste0(yr,' QB Metrics as of ', week_name, ' ', weekday, ' @', time_desc,'  |  min. ', play_min, ' QB Plays')
  )

### calculate rankings
qb_rankings <- full_join(full_hours, full_qb, by = as.character()) %>% 
  left_join(hourly_stats) %>% 
  arrange(actual_datetime_hour) %>% 
  group_by(gsis_id) %>% 
  fill(c(team, last_name, total_plays, epa_per_play, cpoe, dakota)) %>% 
  ungroup %>% 
  left_join(hours_detail) %>% 
  filter(total_plays >= play_min & !is.na(dakota)) %>% 
  arrange(-dakota) %>% 
  group_by(actual_datetime_hour) %>% 
  mutate(
    rank = row_number(),
    active = ifelse(is.na(active), 'no', 'yes')
  ) %>% 
  ungroup %>% 
  full_join(hours_detail) %>% 
  arrange(actual_datetime_hour) %>% 
  mutate(team = ifelse(gsis_id == '00-0035232', 'WAS', team)) 

qb_rankings %>% 
  group_by(actual_datetime_hour) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

subtitle_vec <- hours_detail$full_desc

col_loc <- c(0.5,1.4,2.7,4.1,5.1,6.2,7.3)

col_headers <- data.frame(
  x = c(col_loc,col_loc+8),
  y = 0,
  label = rep(c('Rk', '', 'Player', 'Plays', 'EPA/p', 'CPOE', 'Index'), 2),
  gsis_id = NA
)

max_rank <- qb_rankings %>% filter(!is.na(rank)) %>%  pull(rank) %>% max(.)/2
max_rank <- ceiling(max_rank)

#logos <- nflfastR::teams_colors_logos %>% select(team = team_abbr, logo_url = team_logo_wikipedia)
logos <- qb_rankings %>% 
  filter(!is.na(team)) %>% 
  select(team) %>%
  distinct %>% 
  mutate(grob = grob_img_adj(paste0('local-img/', team, '.png')))

p <- qb_rankings %>% 
  mutate(
    rank_text = paste0('#',rank),
    rank_grp = ifelse(rank > max_rank, 8, 0),
    rank_row = rank %% max_rank,
    rank_row = ifelse(rank_row == 0, max_rank, rank_row),
    cpoe_lab = plus_lab(round(cpoe/100,3), suffix = '%', accuracy = 0.1),
    epa_lab = plus_lab(round(epa_per_play,2), accuracy = .01),
    dakota_lab = number(dakota, accuracy = .001),
    plays_lab = number(total_plays, accuracy = 1),
    active = ifelse(is.na(active), 'no', active),
  ) %>% 
  #left_join(logos) %>% 
  #filter(date_time_index < 20) %>%
  ggplot(aes(y = rank_row, group = gsis_id)) +
  geom_text(data = col_headers, aes(x = x, y = y, label = label), size = 9, color = 'darkblue', family = font_SB) +
  geom_rect(aes(ymin = rank_row - 0.4, ymax = rank_row + 0.4, xmin = 0.05 + rank_grp, xmax = 7.95 + rank_grp, fill = active), show.legend = F) +
  geom_text(aes(x = 0.5 + rank_grp, label = rank_text), size = 9, color = 'darkblue', family = font_SB) +
  #geom_grob(aes(x = 1.4 + rank_grp, y = rank_row, label = grob), vp.height = 0.05) +
  geom_image(aes(x = 1.4 + rank_grp, image = paste0('local-img/', team, '.png')), size = 0.025, asp = 2.15) + 
  geom_text(aes(x = 2.7 + rank_grp, label = last_name), size = 9, color = 'darkblue', family = font_SB) +
  geom_text(aes(x = 4.1 + rank_grp, label = plays_lab), size = 9, color = 'darkblue', family = font_SB) +
  geom_text(aes(x = 5.1 + rank_grp, label = epa_lab), size = 9, color = 'darkblue', family = font_SB) +
  geom_text(aes(x = 6.2 + rank_grp, label = cpoe_lab), size = 9, color = 'darkblue', family = font_SB) +
  geom_text(aes(x = 7.3 + rank_grp, label = dakota_lab), size = 9, color = 'darkblue', family = font_SB) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_reverse(expand = expansion(mult = 0.04)) +
  scale_fill_manual(values = c('no' = 'grey85', 'yes'='skyblue')) +
  vid_theme_SB +
  labs(title = paste0(yr,' Hourly QB Rankings'),
       subtitle = '{subtitle_vec[as.numeric(previous_state)]}'
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(24,24,0,24),'points')
  ) +
  transition_states(date_time_index, transition_length = 4, state_length = 1, wrap = F) +
  enter_fade() +
  exit_fade() +
  ease_aes('linear')

anim_save(filename = 'video/2021 hourly qb rank.mp4', animation = animate_SB(p, data_home = 'Data: @nflfastR', nframes = 4000, fps = 40, height = 900, width = 1600, renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "video/2021 hourly qb rank.mp4" -map 0 -c:v libx264 -c:a copy -y "video/2021 hourly qb rank working.mp4"')

system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "video/2021 hourly qb rank not working FOX.mp4" -map 0 -c:v libx264 -c:a copy -y "video/2021 hourly qb rank working FOX.mp4"')


### fun facts
qb_rankings %>% 
  group_by(gsis_id, last_name) %>% 
  summarise(
    min_rnk = min(rank),
    max_rnk = max(rank)
  ) %>% view

pbp_df %>% 
 filter(!is.na(down) & !is.na(id) & play == 1) %>% 
  group_by(passer) %>% 
  summarise(epa = mean(qb_epa, na.rm = T)) %>% 
  arrange(-epa) %>% 
  mutate(rank = row_number())

