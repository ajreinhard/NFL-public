library(rvest)
library(tidyverse)
library(magrittr)

get_pfr_game_ids <- function(year){
  
  cleaned_names <- c(
    "week",
    "day",
    "date",
    "time",
    "winner_tie",
    "at",
    "loser_tie",
    "boxscore",
    "pts_win",
    "pts_lose",
    "yds_win",
    "to_win",
    "yds_lose",
    "to_lose"
  )
  
  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{year}/games.htm")
  
  raw_html <- read_html(raw_url)
  
  get_boxscore <- function(row_id){
    
    raw_html %>% 
      html_node("#games") %>% 
      html_node("tbody") %>% 
      html_node(glue::glue("tr:nth-child({row_id})")) %>% 
      html_node("td:nth-child(8) > a") %>% 
      html_attr("href") %>% 
      str_remove("/boxscores/") %>% 
      str_remove("\\.htm")
    
  }
  
  raw_table <- raw_html %>% 
    html_node("#games") %>% 
    html_table() %>% 
    set_names(nm = cleaned_names) %>% 
    mutate(row_id = row_number()) %>% 
    filter(week != "Week" & week != "") %>% 
    mutate(
      boxscore = map_chr(row_id, get_boxscore),
      season = as.numeric(gsub('_AFL','',year))
    )
  
  tibble(raw_table)
  
}

get_starters <- function(game_id) {
  
  game_html <- read_html(paste0('https://pro-football-reference.com/boxscores/', game_id, '.htm'))
  
  teams <- game_html %>%
    html_nodes(xpath = '//div[@id="all_team_stats"]//comment()') %>% 
    html_text %>%
    paste(collapse = '') %>%
    read_html %>% 
    html_nodes(xpath = '//@aria-label') %>% 
    html_text %>% 
    .[c(2:3)]
  
  starter_ids <- game_html %>%
    html_nodes(xpath = '//div[@id="all_home_starters"]//comment()') %>% 
    html_text %>%
    paste(collapse = '') %>%
    read_html %>% 
    html_nodes(xpath = '//@data-append-csv') %>%
    html_text
  
  starter_info <- game_html %>%
    html_nodes(xpath = '//div[@id="all_home_starters"]//comment()') %>% 
    html_text %>%
    paste(collapse = '') %>%
    read_html %>% 
    html_nodes(xpath = '//table[@id="home_starters"]') %>% 
    html_table(header = F) %>% 
    extract2(1)
  
  names(starter_info) <- tolower(apply(starter_info[1,], 2, paste, collapse = ''))
  
  home_starters <- starter_info %>%
    filter(!player %in% c('','Player')) %>% 
    mutate(
      game_id = game_id,
      player_id = starter_ids,
      team_type = 'home',
      team = teams[2]
    )
  
  starter_ids <- game_html %>%
    html_nodes(xpath = '//div[@id="all_vis_starters"]//comment()') %>% 
    html_text %>%
    paste(collapse = '') %>%
    read_html %>% 
    html_nodes(xpath = '//@data-append-csv') %>%
    html_text
  
  starter_info <- game_html %>%
    html_nodes(xpath = '//div[@id="all_vis_starters"]//comment()') %>% 
    html_text %>%
    paste(collapse = '') %>%
    read_html %>% 
    html_nodes(xpath = '//table[@id="vis_starters"]') %>% 
    html_table(header = F) %>% 
    extract2(1)
  
  names(starter_info) <- tolower(apply(starter_info[1,], 2, paste, collapse = ''))
  
  away_starters <- starter_info %>%
    filter(!player %in% c('','Player')) %>% 
    mutate(
      game_id = game_id,
      player_id = starter_ids,
      team_type = 'away',
      team = teams[1]
    )
  
  
  bind_rows(home_starters, away_starters)
  
}

for (i in 2009:2020) {
  i %>% 
    get_pfr_game_ids %>% 
    pull(boxscore) %>% 
    lapply(., get_starters) %>%
    bind_rows %>% 
    saveRDS(paste0('pfr/starters/',i,'.rds'))
}

source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

all_box <- lapply(dir('pfr/starters', full.names = T), readRDS) %>% bind_rows %>% distinct
all_games <- lapply(1970:2020, get_pfr_game_ids) %>% bind_rows %>% distinct

elo_df <- get_538elo() %>% team2fran

## dallas texans franchise from before 1952 is seperate, after is cowboys
## 1950 colts are not current colts
## DTX to KC
elo_df <- elo_df %>% 
  mutate_at(
    .vars = vars(ends_with('team')),
    season = elo_df %>% pull(season),
    .funs = function(tm, season) {
      case_when(
        tm == 'NYY' ~ 'DTX',
        tm == 'BCL' ~ 'BCT',
        TRUE ~ tm
      )
    }
  ) %>% 
  double_games

box_df <- all_games %>% 
  mutate(
    date = ifelse(boxscore == '196509250ram', '1965-09-25', date),
    gameday = as.Date(date)
  ) %>% 
  select(game_id = boxscore, gameday, season) %>% 
  distinct %>% 
  left_join(all_box) %>% 
  rename(tm = team) %>% 
  mutate(
    tm = case_when(
      tm == 'BAL' & season < 1984 ~ 'IND',
      tm == 'BCL' ~ 'BCT',
      tm == 'BOS' ~ 'NE',
      tm == 'CRD' ~ 'ARI',
      tm == 'DTX' & season > 1959 ~ 'KC',
      tm == 'GNB' ~ 'GB',
      tm == 'HOU' & season < 1997 ~ 'TEN',
      tm == 'KAN' ~ 'KC',
      tm == 'LAR' ~ 'LA',
      tm == 'LVR' ~ 'LV',
      tm == 'NOR' ~ 'NO',
      tm == 'NWE' ~ 'NE',
      tm == 'NYY' ~ 'DTX',
      tm == 'NYT' ~ 'NYJ',
      tm == 'OAK' ~ 'LV',
      tm == 'PHO' ~ 'ARI',
      tm == 'RAI' ~ 'LV',
      tm == 'RAM' ~ 'LA',
      tm == 'SDG' ~ 'LAC',
      tm == 'SFO' ~ 'SF',
      tm == 'STL' & season < 1988 ~ 'ARI',
      tm == 'STL' & season > 1994 ~ 'LA',
      tm == 'TAM' ~ 'TB',
      T ~ tm
    )
  ) %>% 
  rename(pfr_game_id = game_id, team = tm) %>% 
  left_join(elo_df)

box_df %>% 
  group_by(game_id, team_type) %>% 
  mutate(n = n()) %>% 
  filter(n == 22) %>% 
  mutate(side = ifelse(row_number() <= 11, 'offense', 'defense')) %>% 
  ungroup %>% 
  group_by(side, pos) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = side, values_from = n)
  #write.csv('pfr/pos map.csv', row.names = F)

pos_map <- read_csv('pfr/pos map.csv')

box_df %>% 
  left_join(pos_map) %>% 
  mutate(side = ifelse(pos == '', 'defense', side)) %>% 
  select(pfr_game_id, game_id, game_type, week, gameday, season, player_id, player, side, pos, team, opp) %>% 
  saveRDS('pfr/data/starters.rds')

starter_df <- readRDS('pfr/data/starters.rds')

starter_df %>% 
  group_by(game_id, team, side) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

week1_starters <- starter_df %>% filter(week == 1 & side == 'offense')

games_started <- starter_df %>% 
  filter(game_type == 'REG') %>% 
  group_by(season, team, player_id) %>% 
  summarise(prev_starts = n()) %>% 
  ungroup

return_all_starters <- week1_starters %>% 
  left_join(games_started %>% mutate(season = season + 1)) %>% 
  mutate(prev_starts = ifelse(is.na(prev_starts), 0, prev_starts)) %>% 
  group_by(season, team) %>% 
  summarise(
    ret_ply = sum(ifelse(prev_starts >= 6, 1, 0)),
    starters = n()
  ) %>% 
  ungroup %>% 
  filter(ret_ply == starters) %>% 
  select(season, team)


prior_szn <- elo_df %>% 
  filter(game_type == 'REG') %>% 
  inner_join(return_all_starters %>% mutate(season = season - 1)) %>% 
  group_by(season, team) %>% 
  summarise(
    games = n(),
    wins = sum(ifelse(result > 0, 1, ifelse(result == 0, 0.5, 0))),
    pt_diff = sum(result) / games,
    .groups = 'drop'
  )

return_szn <- elo_df %>% 
  filter(game_type == 'REG') %>% 
  inner_join(return_all_starters) %>% 
  group_by(season, team) %>% 
  summarise(
    games = n(),
    wins = sum(ifelse(result > 0, 1, ifelse(result == 0, 0.5, 0))),
    pt_diff = sum(result) / games,
    .groups = 'drop'
  )

  
p <- prior_szn %>% 
  mutate(season = season + 1) %>% 
  left_join(return_szn, by = c('team', 'season'), suffix = c('_pri', '_ret')) %>% 
  fran2team %>% 
  hist_logo_url %>% 
  ggplot(aes(x = pt_diff_pri, y = pt_diff_ret, label = paste0('\'',substr(season,3,4)))) +
  geom_abline(color = color_SB[1], linetype = '52', size = 0.6) +
  geom_grob(aes(x = pt_diff_pri, y = pt_diff_ret, label = grob_img_adj(logo_url, alpha = 0.75), vp.height = 0.045)) +
  geom_text_repel(color = 'darkblue', bg.color = 'white', size = 1.8, family = font_SB, point.padding = 0.01, box.padding = 0.05, bg.r = 0.15) +
  geom_shadowtext(x = 12.5, y = 11.5, label = 'Better Before', angle = 45, color = color_SB[1], family = font_SB, size = 2.7, bg.r = .15, bg.color = 'white') +
  geom_shadowtext(x = 11.5, y = 12.5, label = 'Better After', angle = 45, color = color_SB[1], family = font_SB, size = 2.7, bg.r = .15, bg.color = 'white') +
  scale_x_continuous(limits = c(-10,20), expand = expansion(add = 0), breaks = seq(-10,20,5), labels = plus_lab_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-10,20), expand = expansion(add = 0), breaks = seq(-10,20,5), labels = plus_lab_format(accuracy = 1)) +
  labs(title = 'Running it Back',
       subtitle = 'Teams who started 11 Offensive players in Week 1 with at least 5 starts\nfor that team in the prior season, 1970-2020',
       x = 'Average Point Differential in Previous Season',
       y = 'Avg Pt\nDiff in\nReturn\nSeason') +
  theme_SB +
  theme(
    plot.margin = unit(c(7.5,10,7.5,7.5), 'points')
  )

brand_plot(p, asp = 1, save_name = 'graphs/off return starters.png', data_home = 'Data: Pro Footbll Reference', fade_borders = 'tr')
