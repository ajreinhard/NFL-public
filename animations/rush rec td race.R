source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(ggstance)
library(ggtext)

complete_box <- readRDS('pfr/data/boxscores.rds')
hof_df <- readRDS('pfr/data/hof.rds') %>% select(player_id, HOF_yr = indct)

all_weeks <- complete_box %>% 
  filter(season >= 1966) %>% 
  select(season, week) %>% 
  distinct %>% 
  mutate(
    week_order = row_number(),
    week_name = paste0('As of Week ',week, ', ', season)
  )

td_df <- complete_box %>% 
  left_join(all_weeks) %>% 
  mutate(week_order = ifelse(is.na(week_order), 0, week_order)) %>% 
  group_by(week_order, player_id, team) %>% 
  summarise(rush_rec_td = sum(rush_td) + sum(rec_td)) %>% 
  ungroup

all_player_weeks <- td_df %>% 
  group_by(player_id) %>% 
  summarise(
    final_week = max(week_order),
    week_order = min(week_order):max(all_weeks$week_order),
    retired = ifelse(final_week < week_order & final_week <= max(all_weeks$week_order) - 21, 1, 0)
  ) %>% 
  ungroup %>% 
  left_join(all_weeks) %>% 
  left_join(td_df) %>% 
  group_by(player_id, season) %>% 
  fill(team, .direction = 'downup') %>% 
  group_by(player_id) %>% 
  fill(team, .direction = 'downup') %>% 
  mutate(car_rush_rec_td = cumsum(ifelse(is.na(rush_rec_td), 0, rush_rec_td))) %>% 
  ungroup

sb_era_players <- complete_box %>% 
  filter(season >= 1966) %>% 
  select(player_id, player) %>% 
  distinct

every_team_logo <- expand.grid(team = .tm_div_order, season = 1950:2020) %>% 
  hist_logo_url %>% 
  select(logo_url) %>% 
  distinct %>% 
  mutate(file_name = gsub('https://a.espncdn.com/i/teamlogos/nfl/500/','',gsub('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/','',logo_url)))

retire_df <- complete_box %>% 
  group_by(player_id, team) %>% 
  summarise(n = n(), season = max(season)) %>% 
  arrange(-n) %>% 
  group_by(player_id) %>% 
  filter(row_number() == 1) %>% 
  ungroup %>% 
  hist_logo_url %>% 
  left_join(every_team_logo) %>% 
  select(player_id, ret_logo = file_name)

# full_rankings <- all_player_weeks %>% 
#   inner_join(sb_era_players) %>% 
#   group_by(week_order) %>% 
#   arrange(-car_rush_rec_td) %>% 
#   mutate(rank = row_number()) %>% 
#   ungroup %>% 
#   filter(rank <= 20)
# 
# full_rankings %>% 
#   filter(player_id == 'RiceJe00') %>% 
#   arrange(week_order)
# 
# full_rankings %>% filter(week_order == 1)

viz_df <- all_player_weeks %>% 
  inner_join(sb_era_players) %>% 
  group_by(week_order) %>% 
  arrange(-car_rush_rec_td) %>% 
  mutate(
    rank = row_number(),
    leader = max(car_rush_rec_td)
  ) %>% 
  ungroup %>% 
  filter(rank <= 20) %>% 
  hist_color_map %>% 
  hist_logo_url %>% 
  left_join(all_weeks) %>% 
  left_join(every_team_logo) %>% 
  left_join(retire_df) %>% 
  left_join(hof_df) %>% 
  mutate(
    logo_url = paste0('C:/Users/Owner/Documents/nflfastR-data/logo-adj/',ifelse(retired == 1, paste0('small-bw/', ret_logo), paste0('regular/', file_name))),
    status = case_when(
      season >= HOF_yr ~ 'Hall',
      retired == 1 ~ 'Retired',
      T ~ 'Active'
    ),
    color_key = ifelse(retired == 1, 'RET', color_key)
  ) %>% 
  filter(week_order > 0)

#viz_df %>% filter(week_order == 0) %>% view

addtl_end_time <- tibble(week_order = max(all_weeks$week_order) + 1:50)
addtl_start_time <- tibble(week_order = -(1:30 - 1))

end_freeze <- viz_df %>% 
  filter(week_order == max(week_order)) %>% 
  mutate(week_order = NULL) %>% 
  full_join(addtl_end_time, by = character())

start_freeze <- viz_df %>% 
  filter(week_order == 1) %>% 
  mutate(week_order = NULL) %>% 
  full_join(addtl_start_time, by = character())

viz_df <- viz_df %>% 
  bind_rows(start_freeze) %>% 
  bind_rows(end_freeze)


my_ani <- viz_df %>% 
  ggplot(aes(x = car_rush_rec_td, y = rank, group = player_id)) +
  geom_barh(aes(color = color_key, fill = color_key), stat = 'identity', show.legend = F, size = 2, width = 0.8, alpha = 0.8) +
  geom_richtext(aes(label = paste0(ifelse(status == 'Hall', '**',''),player, ' (',car_rush_rec_td,')',ifelse(status == 'Hall', '**','')), x = car_rush_rec_td - leader * 0.005, color = status), hjust = 1, size = 11, family = 'MS Reference Sans Serif', show.legend = F, label.colour = 'transparent', fill = 'transparent', label.size = 0, nudge_y = -0.05) +
  geom_image(aes(image = logo_url, x = car_rush_rec_td + leader * 0.02), asp = 2.03, size = 0.03) +
  geom_richtext(aes(x = leader * .9, y = ifelse(rank == 1, 15.5, NA)), label = '<p>HOF Inductees in <b><span style="color:#6d5e00">GOLD</span></b></p>', text.colour = '#00008B', family = font_SB, size = 12, fill = 'grey70', label.padding = unit(c(1.2,1.1,1.1,1.4), "lines"), label.colour = '#00008B', label.size = 1.4, label.r = unit(0.5, "lines")) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04)), breaks = seq(0,250,10), labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c(NFL_pri, 'RET' = 'grey70')) +
  scale_color_manual(values = c(NFL_sec, 'RET' = 'grey40', 'Active' = 'white', 'Retired' = 'grey20', 'Hall' = '#6d5e00')) +
  labs(title = 'Most Rushing & Receiving Touchdowns by Super Bowl Era Players',
       subtitle = '{all_weeks$week_name[as.numeric(ifelse(as.numeric(previous_state) < 1, 1, ifelse(as.numeric(previous_state) > max(all_weeks$week_order), max(all_weeks$week_order), previous_state)))]}  |  min. 1 Game in Super Bowl Era (1966-Present)',
       x = NULL,
       y = NULL) +
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

## 1920x1080 and 5600 frames at 40fps is probably best quality
anim_save(filename = 'graphs/rec rush tds.mp4', animation = animate_SB(my_ani, nframes = 5600, fps = 40, height = 1080, width = 1920, data_home = 'Data: Pro Football Reference', renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/rec rush tds.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/rec rush tds working.mp4"')
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "graphs/rec rush tds w music.mp4" -map 0 -c:v libx264 -c:a copy -y "graphs/rec rush tds w music working.mp4"')


viz_df %>% 
  group_by(team) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  view
  


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
