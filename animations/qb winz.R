setwd('C:/Users/Owner/Documents/GitHub/NFL-public')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(ggstance)
library(ggtext)

HOF_yr <- read_csv('C:/Users/Owner/Documents/GitHub/NFL/NFL/HOF QBs.csv')
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

### locally saved team images lookup
every_team_logo <- expand.grid(team = .tm_div_order, season = 1966:2020) %>% 
  hist_logo_url %>% 
  select(logo_url) %>% 
  distinct %>% 
  mutate(file_name = gsub('https://a.espncdn.com/i/teamlogos/nfl/500/','',gsub('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/','',logo_url)))

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
  left_join(every_team_logo) %>% 
  mutate(ret_logo = paste0('C:/Users/Owner/Documents/nflfastR-data/logo-adj/small-bw/',file_name)) %>% 
  select(team_qb, ret_logo)

viz_df <- qb_wins %>% 
  left_join(weekly_qb_team) %>% 
  group_by(team_qb) %>% 
  mutate(retire_season = max(ifelse(!is.na(team), season + 1, NA), na.rm = T)) %>% 
  ungroup %>% 
  filter(rank <= 20) %>% 
  team2fran %>% 
  left_join(HOF_yr) %>% 
  hist_color_map %>% 
  hist_logo_url %>% 
  left_join(retire_logo) %>% 
  left_join(every_team_logo) %>% 
  mutate(
    logo_url = paste0('C:/Users/Owner/Documents/nflfastR-data/logo-adj/regular/',file_name),
    status = case_when(
      season >= HOF_year ~ 'Hall',
      season >= retire_season ~ 'Retired',
      T ~ 'Active'
    ),
    color_key = ifelse(season >= retire_season, 'RET', color_key),
    logo_url = ifelse(season >= retire_season, ret_logo, logo_url)
  )

addtl_end_time <- tibble(week_order = max(all_weeks$week_order) + 1:30)
addtl_start_time <- tibble(week_order = -(1:30 - 1))

newly_ret <- c('Drew Brees', 'Philip Rivers')
newly_HOF <- c('Peyton Manning')

end_freeze <- viz_df %>% 
  filter(week_order == max(week_order)) %>% 
  mutate(week_order = NULL) %>% 
  full_join(addtl_end_time, by = character()) %>% 
  mutate(
    logo_url = ifelse(team_qb %in% newly_ret, ret_logo, logo_url),
    color_key = ifelse(team_qb %in% newly_ret, 'RET', color_key),
    status = ifelse(team_qb %in% newly_ret, 'Retired', status),
    status = ifelse(team_qb %in% newly_HOF, 'Hall', status)
  )

start_freeze <- viz_df %>% 
  filter(week_order == 1) %>% 
  mutate(week_order = NULL) %>% 
  full_join(addtl_start_time, by = character())

viz_df <- viz_df %>% 
  bind_rows(start_freeze) %>% 
  bind_rows(end_freeze)

lombardi_img <- qb_wins %>% 
  filter(rank <= 20) %>% 
  full_join(tibble(sb_cnt = 1:max(qb_wins$sb_wins)), by = character()) %>% 
  filter(sb_wins >= sb_cnt) %>% 
  select(week_order, team_qb, rank, wins, wins_leader, sb_cnt) %>% 
  mutate(x_loc = wins - sb_cnt * (wins_leader * 0.015) + wins_leader * 0.005)

lombardi_img <- lombardi_img %>% 
  filter(week_order == max(week_order)) %>% 
  mutate(week_order = NULL) %>% 
  full_join(addtl_end_time, by = character()) %>% 
  bind_rows(., lombardi_img)


legend_placement <- viz_df %>% select(week_order, wins_leader) %>% mutate(team_qb = 'empty') %>% distinct


my_ani <- viz_df %>% 
  #filter(week_order > 1039) %>% 
  ggplot(aes(x = wins, y = rank, group = team_qb)) +
  geom_barh(aes(color = color_key, fill = color_key), stat = 'identity', show.legend = F, size = 2, width = 0.8, alpha = 0.8) +
  geom_richtext(aes(label = paste0(ifelse(status == 'Hall', '**',''),team_qb, ' (',wins,')',ifelse(status == 'Hall', '**','')), x = wins - wins_leader * 0.005 - sb_wins * (wins_leader * 0.015), color = status), hjust = 1, size = 11, family = 'MS Reference Sans Serif', show.legend = F, label.colour = 'transparent', fill = 'transparent', label.size = 0) +
  geom_image(aes(image = logo_url, x = wins + wins_leader * 0.02), asp = 2.03, size = 0.03) +
  geom_image(data = lombardi_img, aes(image = 'C:/Users/Owner/Documents/GitHub/NFL/NFL/lomb.png', x = x_loc), asp = 2.03, size = 0.015) +
  geom_richtext(data = legend_placement, aes(x = wins_leader * .9, y = 15.5), label = '<p style="text-indent:20px">= Super Bowl Victory<br>HOF Inductees in <b><span style="color:#6d5e00">GOLD</span></b></p>', text.colour = '#00008B', family = font_SB, size = 12, fill = 'grey70', label.padding = unit(c(1.2,1.1,1.1,1.4), "lines"), label.colour = '#00008B', label.size = 1.4, label.r = unit(0.5, "lines")) +
  geom_image(data = legend_placement, aes(image = 'C:/Users/Owner/Documents/GitHub/NFL/NFL/lomb.png', x = wins_leader * .814, y = 15), asp = 2.03, size = 0.03) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04)), breaks = seq(0,300,20), labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c(NFL_pri, 'RET' = 'grey70')) +
  scale_color_manual(values = c(NFL_sec, 'RET' = 'grey40', 'Active' = 'white', 'Retired' = 'grey20', 'Hall' = '#6d5e00')) +
  labs(title = 'Wins as a Starting QB by Super Bowl Era Players',
       subtitle = '{all_weeks$week_name[as.numeric(ifelse(as.numeric(previous_state) < 1, 1, ifelse(as.numeric(previous_state) > max(all_weeks$week_order), max(all_weeks$week_order), previous_state)))]}  |  min. 1 QB Start in Super Bowl Era (1966-Present)',
       x = NULL,
       y = 'QB Winz') +
  theme(
    line = element_line(lineend = 'round', color='darkblue'),
    text = element_text(family = font_SB, color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'transparent'),
    panel.border = element_rect(color = 'darkblue', fill = NA),
    panel.background = element_rect(fill = 'white', color = 'transparent'),
    axis.ticks = element_line(color = 'darkblue', size = 1.8),
    axis.ticks.length = unit(10, 'pt'),
    axis.title = element_text(size = 29),
    axis.text = element_text(size = 25, color = 'darkblue'),
    plot.title = element_text(size = 51),
    plot.subtitle = element_text(size = 29),
    plot.caption = element_text(size = 18),
    legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='grey70', size = 1.1),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 23, color = 'darkblue', family = font_SB)
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color='grey70', size = 1.4),
    axis.line = element_line(color = 'darkblue', size = 1.8)
  ) +
  transition_states(week_order, transition_length = 4, state_length = 1, wrap = F) +
  view_follow(fixed_y = T) +
  enter_fly(y_loc = -21) +
  exit_fly(y_loc = -21) +
  ease_aes('linear')


anim_save(filename = 'graphs/qb winz.mp4', animation = animate_SB(my_ani, nframes = 5000, fps = 40, data_home = 'Data: FiveThirtyEight', height = 1080, width = 1920, renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
#anim_save(filename = 'graphs/qb winz short.mp4', animation = animate(my_ani, nframes = 200, fps = 40, height = 1080, width = 1920, renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/qb winz.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/qb winz working.mp4"')
#system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/qb winz short.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/qb winz short working.mp4"')

system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/qb winz with classic.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/qb winz with classic working.mp4"')
# used to create branded videos
Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- ggplot_gtable(plot)
    
    # insert changes here
    logo_file <- magick::image_read('C:/Users/Owner/Documents/GitHub/data-viz/ggplot/statbutler.png')
    
    author_txt <- textGrob('By Anthony Reinhard', x=unit(0.065, 'npc'), gp=gpar(col='darkblue', fontfamily=font_SB, fontsize=24), hjust=0)
    data_txt <- textGrob(self$data_home, x=unit(1 - (.01), 'npc'), gp=gpar(col='grey95', fontfamily=font_SB, fontsize=24), hjust=1)
    footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
    footer <- grobTree(footer_bg, author_txt, data_txt)
    
    plt.final <- grid.arrange(plot, footer, heights=unit(c(1, 44), c('null','pt')))
    plot <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002, y = 0, hjust = 0, vjust = 0, height = 0.08, width = 0.1067 * (9/16))
    
    if (!is.null(widths)) plot$widths <- widths
    if (!is.null(heights)) plot$heights <- heights
    if (newpage) grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid.draw(plot)
    } else {
      if (is.character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  }
)

Scene2$data_home <- NULL

### the next four functions will simply duplicate existing nested gganimate functions and replace them with my personalized Scene2 function
# used to create branded videos
create_scene2 <- function(transition, view, shadow, ease, transmuters, nframes, data_home) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene2, transition = transition, 
          view = view, shadow = shadow, ease = ease, 
          transmuters = transmuters, nframes = nframes,
          data_home = data_home)
}

# used to create branded videos
ggplot_build2 <- gganimate:::ggplot_build.gganim
formals(ggplot_build2) <- c(formals(ggplot_build2), alist(data_home = ))
body(ggplot_build2) <- body(ggplot_build2) %>%
  as.list() %>%
  inset2(4,
         quote(scene <- create_scene2(plot$transition, plot$view, plot$shadow, 
                                      plot$ease, plot$transmuters, plot$nframes, data_home))) %>%
  as.call()


# used to create branded videos
prerender2 <- gganimate:::prerender
formals(prerender2) <- c(formals(prerender2), alist(data_home = ))
body(prerender2) <- body(prerender2) %>%
  as.list() %>%
  inset2(3,
         quote(ggplot_build2(plot, data_home))) %>%
  as.call()


# used to create branded videos
animate_SB <- gganimate:::animate.gganim
formals(animate_SB) <- c(formals(animate_SB)[-length(formals(animate_SB))], alist(data_home = ''), formals(animate_SB)[length(formals(animate_SB))])
body(animate_SB) <- body(animate_SB) %>%
  as.list() %>%
  inset2(8,
         quote(plot <- prerender2(plot, nframes_total, data_home))) %>%
  as.call()


#1920 x 1080
1920/1600
1.2 * 8

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


