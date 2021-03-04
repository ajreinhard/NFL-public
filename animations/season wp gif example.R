library(nflfastR)
library(tidyverse)
library(gifski)
library(ggpmisc)
library(magick)
library(scales)
library(grid)
library(cowplot)

### this script will create a win probability graph for every game this season and then put them all into a GIF
### create a folder called "WP" in your directory before starting. You can delete the contents after

### get full season pbp
pbp_df <- nflfastR::load_pbp(2020)

### info from every game
games_df <- readRDS(url('http://nflgamedata.com/games.rds')) %>%
  filter(season==2020) %>% 
  mutate(
    game_hr = as.numeric(format(strptime(gametime, format = '%H'),'%I')),
    gm_time = paste0(weekday,' ',as.character(format(strptime(gametime, '%R'), '%l:%M %p')), ' ET'),
    gm_note = case_when(
      stadium_id %in% c('LON00','LON02') ~ 'London Game',
      stadium_id %in% c('MEX00') ~ 'Mexico City Game',
      game_type == 'SB' ~ 'Super Bowl LV',
      game_type == 'CON' ~ 'Conf Championship Game',
      game_type == 'DIV' ~ 'Divisional Playoffs',
      game_type == 'WC' ~ 'Wild Card Playoffs',
      gameday == '2020-11-26' ~ 'Thanksgiving Day Game',
      TRUE ~ ''
    )
  )

### my personalized function to render images as grobs
### ggimage is easier to use and does not require use of this function
grob_img_adj <- function(img_url, alpha = 1, whitewash = 0, boost_color = T, bw = F) {
  return(lapply(img_url, function(x) {
    if(is.na(x)) {
      return(NULL)
    } else {     
      img <- image_read(x)[[1]]
      
      if (bw) {
        grey_scale <- as.integer(img[1,,]) * 0.3 + as.integer(img[2,,]) * 0.59 + as.integer(img[3,,]) * 0.11
        img[1,,] <- as.raw(grey_scale)
        img[2,,] <- as.raw(grey_scale)
        img[3,,] <- as.raw(grey_scale)
      }
      
      lowest_alpha <- function(x) (255 - as.integer(x)) / 255
      alpha_min <- sapply(1:3, function(x) lowest_alpha(img[x,,])) %>%
        apply(., 1, max) %>% 
        ifelse(. >= alpha, ., alpha)
      
      if(!boost_color) {alpha_min <- 1}
      
      img[1,,] <- as.raw(255 - (255 - as.integer(img[1,,])) * (1-whitewash)  * (1/alpha_min))
      img[2,,] <- as.raw(255 - (255 - as.integer(img[2,,])) * (1-whitewash) * (1/alpha_min))
      img[3,,] <- as.raw(255 - (255 - as.integer(img[3,,])) * (1-whitewash) * (1/alpha_min))
      img[4,,] <- as.raw(as.integer(img[4,,]) * alpha)
      return(grid::rasterGrob(image = image_read(img)))
    }
  }))
}

### my function to retreive the wordmarks that I created
wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
ESPN_logo_url = function(x) paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')

### helps place grobs below
grob_df <- data.frame(y = c(.8,.2))

### loop through each game_id and make a plot
for (gm in games_df$game_id) {
  
  game <- filter(games_df, game_id == gm)
  
  ### get all the plays for just this game
  game_plays_df <- pbp_df %>% 
    filter(game_id == gm & !is.na(vegas_home_wp)) %>% 
    mutate(
      game_seconds_remaining = ifelse(qtr == 5, quarter_seconds_remaining - 601, game_seconds_remaining),
      end_game_wp = ifelse(desc=='END GAME', vegas_home_wp, NA)
    )
  
  ### create the plot  
  p <- ggplot(data = game_plays_df, aes(x = game_seconds_remaining, y = 1 - vegas_home_wp)) +
    geom_rect(aes(xmin = 1800, xmax = 2700, ymin = -Inf, ymax = Inf), fill = 'grey95') +
    geom_rect(aes(xmin = 0, xmax = 900, ymin = -Inf, ymax = Inf), fill = 'grey95') +
    geom_grob(data = grob_df, aes(y = y, x = (3600 - min(game_plays_df$game_seconds_remaining, na.rm = T)) * .1 + min(game_plays_df$game_seconds_remaining, na.rm = T), label = grob_img_adj(ESPN_logo_url(c(game$away_team, game$home_team)), whitewash = 0.6)), vp.height = .55) +
    geom_hline(yintercept = seq(0,1,.25), color = 'grey70', size = 0.5) +
    geom_line(size = 1) +
    geom_point(aes(y = 1-end_game_wp), pch = 16, size = 4, na.rm = T) +
    scale_x_reverse(expand = expansion(add = 1), breaks = seq(3600,-900,-900)) +
    scale_y_continuous(limits = c(0,1), expand = expansion(add = 0.005), label = percent) +
    labs(title = gm,
         subtitle = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = 'grey70'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y.right = element_line(color = 'grey70', size = 0.5, lineend= 'round'),
      plot.title = element_text(size = 40),
      plot.margin = unit(c(0,7.5,12,7.5), unit = 'points')
    )
  
  ### add team wordmarks, score, and game info to the area where the plot title would normally be
  p_bld <- ggplot_gtable(ggplot_build(p))
  away_wordmark <- rasterGrob(image = image_read(wordmark_url(game$away_team)), vp = viewport(height = .35, width = .4), x = 0.97, y = 1.1)
  home_wordmark <- rasterGrob(image = image_read(wordmark_url(game$home_team)), vp = viewport(height = .35, width = .4), x = 0.97, y = -0.1)
  away_score <- textGrob(game$away_score, gp=gpar(fontsize = 18), y = 0.73, x = .95)
  home_score <- textGrob(game$home_score, gp=gpar(fontsize = 18), y = 0.29, x = .95)
  game_desc <- textGrob(paste0(format(as.Date(game$gameday), '%x'),' - Week ',game$week,'\n',game$gm_time,'\n',game$gm_note), gp=gpar(fontsize = 9), y = .9, x = .2, vjust = 1)
  player_tree <- grobTree(away_wordmark, home_wordmark, away_score, home_score, game_desc)
  
  p_bld$grobs[[16]] <- player_tree
  
  ggsave(ggdraw(p_bld), dpi = 'retina', filename = paste0('wp/',gm,'.png'), height = 3, width = 3 * (16/9))
}

### get the file path for every single graph
all_graphs <- dir('wp', full = T)

### put them in a random order and save the gif
gifski(sample(all_graphs, length(all_graphs)), gif_file = "2020 random wp.gif", width = 800, height = 450, delay = 0.04)
