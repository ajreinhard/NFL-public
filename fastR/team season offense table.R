setwd('C:/Users/rei1740/Desktop/Anthony/nfl')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2021.rds')))
games_df <- readRDS(url('http://nflgamedata.com/games.rds'))

my_team <- 'CLE'

game_level_df <- pbp_df %>%
  filter(posteam == my_team & (pass == 1 | rush == 1)) %>% 
  group_by(game_id) %>% 
  summarise(
    pass_oe = plus_lab(mean(pass_oe, na.rm = T) / 100, suffix = '%', accuracy = 0.1),
    pass_epa = plus_lab(mean(ifelse(pass == 1, epa, NA), na.rm = T), accuracy = 0.01),
    rush_epa = plus_lab(mean(ifelse(rush == 1, epa, NA), na.rm = T), accuracy = 0.01),
    cpoe = plus_lab(round(mean(cpoe, na.rm = T) / 100, 3), suffix = '%', accuracy = 0.1)
  )

team_record <- games_df %>% 
  pivot_longer(c(home_team, away_team), names_to = 'team_type', values_to = 'team') %>% 
  filter(team==my_team & season == 2021 & !is.na(result)) %>% 
  mutate(
    result = result * ifelse(team_type == 'home_team', 1, -1),
    win = ifelse(result > 0, 1, 0),
    loss = ifelse(result < 0, 1, 0),
    tie = ifelse(result == 0, 1, 0)
  ) %>% 
  summarise(
    win = paste0(sum(win), 'W'),
    loss = paste0(sum(loss), 'L'),
    tie = paste0(sum(tie), 'T')
  ) %>% 
  mutate(rec = paste0(win, ' - ', loss, ifelse(tie == '0T', '', paste0(' - ', tie)))) %>% 
  pull(rec)
  
season_tot_df <- pbp_df %>%
  filter(posteam == my_team & (pass == 1 | rush == 1)) %>% 
  mutate(
    week = '',
    short_date = '',
    opp = '',
    matchup = '2021 Season',
    game_text = team_record
  ) %>% 
  group_by(week, short_date, opp, matchup, game_text) %>% 
  summarise(
    pass_oe = plus_lab(mean(pass_oe, na.rm = T) / 100, suffix = '%', accuracy = 0.1),
    pass_epa = plus_lab(mean(ifelse(pass == 1, epa, NA), na.rm = T), accuracy = 0.01),
    rush_epa = plus_lab(mean(ifelse(rush == 1, epa, NA), na.rm = T), accuracy = 0.01),
    cpoe = plus_lab(round(mean(cpoe, na.rm = T) / 100, 3), suffix = '%', accuracy = 0.1),
    .groups = 'drop'
  )

teams_colors_logos$team_city <- sapply(1:nrow(teams_colors_logos), function(x) gsub(paste0(' ',teams_colors_logos$team_nick[x]), '', teams_colors_logos$team_name[x]))

games_df %>% 
  filter(season == 2021 & (away_team == my_team | home_team == my_team)) %>% 
  mutate(
    home_away = ifelse(home_team == my_team, 'home', 'away'),
    opp = ifelse(home_away == 'home', away_team, home_team)
  ) %>% 
  left_join(teams_colors_logos, by = c('opp' = 'team_abbr')) %>% 
  left_join(game_level_df) %>% 
  mutate(
    team_score = ifelse(home_away == 'home', home_score, away_score),
    opp_score = ifelse(home_away == 'home', away_score, home_score),
    game_result = ifelse(team_score == opp_score, 'T', ifelse(team_score > opp_score, 'W', 'L')),
    short_date = format(as.Date(gameday), '%m/%d'),
    matchup = ifelse(home_away == 'home', team_city, paste0('@ ',team_city)),
    gametime_clean = tolower(gsub(' ','',format(strptime(gametime, '%H:%M'), '%l:%M%p'))),
    game_text = ifelse(is.na(team_score), gametime_clean, paste0(game_result, '   ', team_score, '-', opp_score)),
    pass_oe = ifelse(is.na(team_score), '', pass_oe),
    pass_epa = ifelse(is.na(team_score), '', pass_epa),
    rush_epa = ifelse(is.na(team_score), '', rush_epa),
    cpoe = ifelse(is.na(team_score), '', cpoe)
  ) %>% 
  select(week, short_date, opp, matchup, game_text, pass_oe, pass_epa, rush_epa, cpoe) %>% 
  rbind(., season_tot_df) %>% 
  gt() %>%
  tab_header(title = paste0('2021 ',teams_colors_logos$team_name[which(teams_colors_logos$team_abbr == my_team)] , ' Offense by Game')) %>% 
  cols_label(
    week = 'Wk',
    short_date = 'Date',
    opp = '',
    matchup = 'Opponent',
    game_text = 'Score',
    pass_oe = html('Dropbacks<br>Over Exp'),
    pass_epa = html('Dropback<br>EPA/play'),
    rush_epa = html('Rushing<br>EPA/play'),
    cpoe = 'CPOE'
  ) %>% 
  tab_style(style = cell_text(font = font_SB, size = 'xx-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(font = font_SB, align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(font = font_SB, align = 'left'), locations = cells_body(vars(matchup))) %>% 
  tab_style(style = cell_text(font = font_SB, align = 'center', size = 'medium'), locations = cells_column_labels(columns = vars(week, short_date, matchup, game_text, pass_oe, pass_epa, rush_epa, cpoe))) %>% 
  tab_style(style = cell_borders(sides  = 'bottom', color = 'darkblue', weight = '2px'), locations = cells_body(rows = 17)) %>% 
  tab_style(style =
    list(
      cell_text(font = font_SB, align = 'center', weight = 'bold', size = px(18)),
      cell_borders(sides  = 'bottom', color = 'transparent', weight = '1px'),
      cell_fill(color = 'grey95')
    ),
    locations = cells_body(rows = 18)
  ) %>% 
  text_transform(
    locations = cells_body(vars(opp), rows = 1:17),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  text_transform(
    locations = cells_body(vars(pass_oe, pass_epa, rush_epa, cpoe)),
    fn = function(x) paste0('<font style="color:',ifelse(grepl('-',x), 'red','darkblue'),'">',x,'</font>')
  ) %>% 
  tab_footnote(
    footnote = html('<font style="font-family:Bahnschrift">Dropbacks Over Expected: Higher = More dropbacks compared to league based on situation</font>'),
    locations = cells_column_labels(
      columns = vars(pass_oe))
  ) %>% 
  tab_footnote(
    footnote = html('<font style="font-family:Bahnschrift">Expected Points Added per Play: Higher = More efficient production compared to league based on situation</font>'),
    locations = cells_column_labels(
      columns = vars(pass_epa, rush_epa))
  ) %>% 
  tab_footnote(
    footnote = html('<font style="font-family:Bahnschrift">Completion Probability Over Expected: Higher = Better completion % compared to league based on pass depth and situation</font>'),
    locations = cells_column_labels(
      columns = vars(cpoe))
  ) %>% 
  table_theme_SB %>% 
  brand_table('team off 2021.png', data_home = 'Data: @nflfastR', base_size = 5)
