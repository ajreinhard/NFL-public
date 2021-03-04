library(nflfastR)
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
    mutate(boxscore = map_chr(row_id, get_boxscore))
  
  tibble(raw_table)
  
}


pfr_team_abbr <- c(
  'ARI' = 'ARI',
  'ATL' = 'ATL',
  'BAL' = 'BAL',
  'BUF' = 'BUF',
  'CAR' = 'CAR',
  'CHI' = 'CHI',
  'CIN' = 'CIN',
  'CLE' = 'CLE',
  'DAL' = 'DAL',
  'DEN' = 'DEN',
  'DET' = 'DET',
  'GNB' = 'GB',
  'IND' = 'IND',
  'JAX' = 'JAX',
  'KAN' = 'KC',
  'MIA' = 'MIA',
  'MIN' = 'MIN',
  'NOR' = 'NO',
  'NWE' = 'NE',
  'NYG' = 'NYG',
  'NYJ' = 'NYJ',
  'OAK' = 'OAK',
  'PHI' = 'PHI',
  'PIT' = 'PIT',
  'SDG' = 'SD',
  'SEA' = 'SEA',
  'SFO' = 'SF',
  'STL' = 'STL',
  'TAM' = 'TB',
  'WAS' = 'WAS',
  'TEN' = 'TEN',
  'HOU' = 'HSO',
  'RAI' = 'LRD',
  'RAM' = 'LRM'
)


get_pfr_pbp <- function(game_id) {

if(game_id == '199711300phi') {return(NULL)}
  
game_html <- read_html(paste0('https://www.pro-football-reference.com/boxscores/', game_id, '.htm'))

# main pbp table
pbp_html <- game_html %>% 
  html_nodes(xpath = '//div[@id="all_pbp"]//comment()') %>% 
  html_text %>%
  paste(collapse = '') %>%
  read_html

box_teams_html <- game_html %>% html_nodes(xpath = '//div[@id="all_player_offense"]//td[@data-stat="team"]') %>% html_text
box_ids_html <- game_html %>% html_nodes(xpath = '//div[@id="all_player_offense"]//@data-append-csv') %>% html_text
team_lookup <- data.frame(player_id = box_ids_html, team = box_teams_html)

# get team names for later. Probably could get first two from games file
away_team_abbr <- box_teams_html %>% unique %>% .[1]
home_team_abbr <- box_teams_html %>% unique %>% .[2]
away_team_full <- game_html %>% html_nodes(xpath = '//strong/a') %>% html_text %>% .[2]
home_team_full <- game_html %>% html_nodes(xpath = '//strong/a') %>% html_text %>% .[1]

# need to simplify this. Just trying to get the roof & spread. Could eventually pull from game file
game_info <- game_html %>% 
  html_nodes(xpath = '//div[@id="all_game_info"]//comment()') %>% 
  html_text %>%
  paste(collapse = '') %>%
  read_html %>% 
  html_table %>% 
  extract2(1)

roof <- game_info %>% filter(X1 == 'Roof') %>% pull(X2)

# trying to suppress NA numeric warning from below
warn = getOption('warn')
options(warn = -1)
spread <- game_info %>% filter(X1 == 'Vegas Line') %>% pull(X2) %>% gsub(away_team_full, '', .) %>% as.numeric
if(is.na(spread)) {spread <- game_info %>% filter(X1 == 'Vegas Line') %>% pull(X2) %>% gsub(home_team_full, '', .) %>% as.numeric * -1}
if(game_info %>% filter(X1 == 'Vegas Line') %>% pull(X2) == 'Pick') {spread <- 0}
options(warn = warn)

# just need to get season. could eventually get from games file
season <- game_html %>% html_nodes(xpath = '//div[@id = "inner_nav"]//li/a') %>% html_text %>% .[2] %>% gsub(' NFL Scores & Schedule', '', .) %>% as.numeric

# get every single row from the pbp data. This includes header rows that are functionally empty. Will remove later
# the first row is removed here, as that is the main header
all_rows <- pbp_html %>% html_nodes(xpath = '//tr') %>% .[-c(1)]

# when the class attribute equals "divider" it signals that there has been a possesion change
pos_swap <- grepl('divider', sapply(all_rows, function(rw) rw %>% html_attr('class')))

# list of player ids
player_id_list <- lapply(all_rows, function(rw) {
  rw %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    .[-c(1:2)] %>% 
    gsub('/.*/(.*)/', '', .) %>% 
    gsub('.htm', '', .)
})

# list of player names
player_name_list <- lapply(all_rows, function(rw) {
  rw %>% 
    html_nodes('a') %>% 
    html_text %>% 
    .[-c(1:2)]
})

# the lists above have incosistent lengths. Make them all the same with NAs
max_players_on_play <- max(sapply(player_id_list, length)) + 1 
for(i in 1:length(player_id_list)) player_id_list[[i]][(length(player_id_list[[i]])+1):max_players_on_play] <- NA
for(i in 1:length(player_name_list)) player_name_list[[i]][(length(player_name_list[[i]])+1):max_players_on_play] <- NA

# surely a better way to do this part, but it works
play_player_ids <- lapply(player_id_list, c) %>%
  do.call(rbind, .) %>% 
  data.frame(stringsAsFactors = F) %>%
  rename_with(function(x) gsub('X', 'action_player_id', x))

play_player_names <- lapply(player_name_list, c) %>%
  do.call(rbind, .) %>% 
  data.frame(stringsAsFactors = F) %>%
  rename_with(function(x) gsub('X', 'action_player_name', x))

# start putting it all together!
pre_posteam_pbp_df <- pbp_html %>% 
  html_table %>% 
  extract2(1) %>% 
  tibble %>% 
  mutate(pos_swap = pos_swap) %>% 
  bind_cols(play_player_ids) %>% 
  bind_cols(play_player_names) %>% 
  # these rows must be removed
  filter(!grepl('Quarter', Quarter) & !grepl('Overtime', Quarter) & !grepl('Regulation', Quarter)) %>% 
  rename(
    qtr = Quarter,
    time_txt = Time,
    down = Down,
    ydstogo = ToGo,
    yrdln = Location,
    desc = Detail,
    pfr_ep = EPB,
    pfr_ep_post = EPA,
    away_score_post = 7,
    home_score_post = 8
  ) %>% 
  mutate_all(function(x) ifelse(x=='', NA ,x)) %>% 
  mutate(time_txt = ifelse(qtr != lag(qtr, default = '1'), '15:00', time_txt)) %>% 
  fill(time_txt) %>% 
  separate(time_txt, ':', into = c('qtr_mins', 'qtr_sec'), convert = T) %>% 
  mutate(
    qtr = as.numeric(ifelse(qtr == 'OT', 5, qtr)),
    game_half = 
      case_when(
        qtr <= 2 ~ 'Half1',
        qtr <= 4 ~ 'Half2',
        qtr > 4 ~ 'Overtime'
      ),
    quarter_seconds_remaining = qtr_mins * 60 + qtr_sec,
    qtr_mins = NULL,
    qtr_sec = NULL,
    half_seconds_remaining = ifelse(qtr == 1 | qtr == 3, quarter_seconds_remaining + 900, quarter_seconds_remaining),
    game_seconds_remaining = ifelse(game_half == 'Half1', half_seconds_remaining + 1800, half_seconds_remaining),
    game_id = game_id,
    play_id = row_number(),
    season = season,
    roof = roof,
    spread_line = spread,
    away_team = away_team_abbr,
    home_team = home_team_abbr,
    down = as.numeric(down),
    ydstogo = as.numeric(ydstogo),
    pfr_ep = as.numeric(pfr_ep),
    pfr_epa = as.numeric(pfr_ep_post) - pfr_ep,
    pfr_ep_post = NULL,
    away_score_post = as.numeric(away_score_post),
    home_score_post = as.numeric(home_score_post),
    total_away_score = lag(away_score_post, default = 0),
    total_home_score = lag(home_score_post, default = 0),
    drive_num = cumsum(pos_swap) + 1,
    pos_swap = NULL,
    play_type = 
     case_when(
       grepl('(no play)', desc) ~ 'no_play',
       grepl(' pass ', desc) | grepl(' sacked ', desc) ~ 'pass',
        grepl(' left for ', desc) | grepl(' right for ', desc) |
        grepl(' left end ', desc) | grepl(' right end ', desc) |
        grepl(' left guard ', desc) | grepl(' right guard ', desc) |
        grepl(' left tackle ', desc) | grepl(' right tackle ', desc) |
        grepl(' middle for ', desc) ~ 'run',
       grepl('punts', desc) ~ 'punt',
       grepl(' kicks off ', desc) ~ 'kickoff',
       grepl(' extra point ', desc) ~ 'extra_point',
       grepl(' field goal ', desc) ~ 'field_goal',
       TRUE ~ 'other'
     ),
    incomplete_pass = ifelse(grepl(' pass incomplete ', desc), 1, 0),
    complete_pass = ifelse(grepl(' pass complete ', desc), 1, 0),
    sack = ifelse(grepl(' sacked ', desc), 1, 0),
    interception = ifelse(grepl(' intercepted ', desc), 1, 0),
    pass_attempt = incomplete_pass + complete_pass + sack,
    two_point_attempt = ifelse(grepl('Two Point Attempt', desc), 1, 0),
    passer_player_id = ifelse(play_type == 'pass', action_player_id1, NA),
    rusher_player_id = ifelse(play_type == 'run', action_player_id1, NA),
    receiver_player_id = ifelse(incomplete_pass == 1 | complete_pass == 1, action_player_id2, NA),
    player_id = ifelse(is.na(passer_player_id), rusher_player_id, passer_player_id),
    passer_player_name = ifelse(play_type == 'pass', action_player_name1, NA),
    rusher_player_name = ifelse(play_type == 'run', action_player_name1, NA),
    receiver_player_name = ifelse(incomplete_pass == 1 | complete_pass == 1, action_player_name2, NA),
    player_name = ifelse(is.na(passer_player_name), rusher_player_name, passer_player_name),
    timeout = ifelse(grepl('Timeout', desc), 1, 0),
    timeout_full_team = ifelse(timeout == 1, substr(desc, 15, nchar(desc)), NA),
    timeout_team = ifelse(timeout_full_team == home_team_full, home_team, away_team),
    timeout_home_team = ifelse(timeout_team == home_team & timeout == 1, 1, 0),
    timeout_away_team = ifelse(timeout_team == away_team & timeout == 1, 1, 0),
    timeout_full_team = NULL
  ) %>% 
  group_by(game_half) %>% 
  mutate(
    away_timeouts_remaining = ifelse(game_half == 'Overtime', 2, 3) - cumsum(timeout_away_team),
    home_timeouts_remaining = ifelse(game_half == 'Overtime', 2, 3) - cumsum(timeout_home_team),
    timeout_away_team = NULL,
    timeout_home_team = NULL
  ) %>% 
  ungroup %>% 
  select(!contains('action_player_'))

# get posteam by looking at team of passer/rusher from box score
posteam_df <- pre_posteam_pbp_df %>% 
  left_join(team_lookup %>% rename(posteam = team), by = c('player_id' = 'player_id')) %>% 
  select(drive_num, game_half, posteam) %>% 
  group_by(game_half, drive_num) %>% 
  fill(posteam, .direction = 'downup') %>% 
  # just take the first play of the drive
  filter(row_number() == 1) %>%
  ungroup %>% 
  group_by(game_half) %>% 
  mutate(
    is_home_pos = posteam == home_team_abbr,
    # ....this is dumb, but if you keep looking backwards/forwards it eventually works for empty possesions
    # most games only need this once
    is_home_pos = ifelse(is.na(is_home_pos), lag(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lag(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lag(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lag(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lag(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lead(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lead(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lead(!is_home_pos), is_home_pos),
    is_home_pos = ifelse(is.na(is_home_pos), lead(!is_home_pos), is_home_pos),
    posteam_type = ifelse(is_home_pos, 'home', 'away'),
    posteam = ifelse(is_home_pos, home_team_abbr, away_team_abbr),
    defteam = ifelse(is_home_pos, away_team_abbr, home_team_abbr),
    is_home_pos = NULL
  ) %>% 
  ungroup

second_half_ko_rec <- posteam_df %>% filter(game_half == 'Half2') %>% pull(posteam) %>% .[1]

pre_posteam_pbp_df %>% 
  left_join(posteam_df, by = c('game_half', 'drive_num')) %>% 
  separate(yrdln, sep = ' ', into = c('side_of_field', 'yardline_50'), remove = F) %>% 
  mutate(
    # PFR switches abbr for these teams, but only for yardline
    side_of_field = case_when(
      side_of_field == 'RAI' & season >= 1995 ~ 'OAK',
      side_of_field == 'CRD' ~ 'ARI',
      side_of_field == 'RAM' & season >= 1995 ~ 'STL',
      side_of_field == 'OTI' & season >= 1997 ~ 'TEN',
      side_of_field == 'OTI' ~ 'HOU',
      side_of_field == 'CLT' ~ 'IND',
      side_of_field == 'RAV' ~ 'BAL',
      TRUE ~ side_of_field
    ),
    yardline_100 = ifelse(posteam == side_of_field, 100 - as.numeric(yardline_50), as.numeric(yardline_50)),
    yardline_50 = NULL,
    posteam_timeouts_remaining = ifelse(home_team == posteam, home_timeouts_remaining, away_timeouts_remaining),
    defteam_timeouts_remaining = ifelse(home_team == defteam, home_timeouts_remaining, away_timeouts_remaining),
    posteam_score = ifelse(home_team == posteam, total_home_score, total_away_score),
    defteam_score = ifelse(home_team == defteam, total_home_score, total_away_score),
    posteam_score_post = ifelse(home_team == posteam, home_score_post, away_score_post),
    defteam_score_post = ifelse(home_team == defteam, home_score_post, away_score_post),
    away_score_post = NULL,
    home_score_post = NULL,
    score_differential = posteam_score - defteam_score,
    receive_2h_ko = ifelse(game_half == 'Half1' & posteam == second_half_ko_rec, 1, 0),
    ### temp change to kickoff plays
    yardline_100 = ifelse(play_type == 'kickoff', 80, yardline_100),
    down = ifelse(play_type == 'kickoff', 1, down),
    ydstogo = ifelse(play_type == 'kickoff', 10, ydstogo)
  ) %>% 
  # no play penalties are causing problems because they are their own plays. throwing them out for now
  filter(!is.na(yrdln)) %>% 
  calculate_expected_points %>% 
  calculate_win_probability %>% 
  group_by(game_half) %>% 
  mutate(
    ### change kickoff plays back 
    yardline_100 = ifelse(play_type == 'kickoff', 30, yardline_100),
    down = ifelse(play_type == 'kickoff', NA, down),
    ydstogo = ifelse(play_type == 'kickoff', NA, ydstogo),
    ### modify PAT ep
    ep = case_when(
      play_type == 'extra_point' ~ 0.969,
      two_point_attempt == 1 ~ 0.947,
      TRUE ~ ep
    ),
    ### figure out how many points were scored on this play
    points_scored = (posteam_score_post - defteam_score_post) - score_differential,
    ### touchdowns are worth 7
    points_scored = case_when(
      points_scored == 6 ~ 7,
      points_scored == -6 ~ -7,
      TRUE ~ points_scored
    ),
    epa = lead(ep, default = 0) * ifelse(lead(posteam) == posteam | is.na(lead(posteam)), 1, -1) - ep,
    epa = ifelse(points_scored == 0, epa, points_scored - ep),
    points_scored = NULL
  ) %>% 
  ungroup %>% 
  # put all the cols in nflfastR order 
  select(play_id, game_id, drive_num, home_team, away_team, posteam, posteam_type, defteam, side_of_field, yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, game_half, qtr, down, yrdln, ydstogo, desc, play_type, home_timeouts_remaining, away_timeouts_remaining, timeout, timeout_team,
         posteam_timeouts_remaining, defteam_timeouts_remaining, total_home_score, total_away_score, posteam_score, defteam_score, score_differential,
         posteam_score_post, defteam_score_post, no_score_prob, opp_fg_prob, opp_safety_prob, opp_td_prob, fg_prob, safety_prob, td_prob, ep, epa, pfr_ep, pfr_epa, wp, vegas_wp,
         incomplete_pass, interception, pass_attempt, sack, two_point_attempt, complete_pass, passer_player_id, passer_player_name, receiver_player_id,
         receiver_player_name, rusher_player_id, rusher_player_name, player_id, player_name, season, spread_line, roof) %>%
  mutate_at(vars(ends_with('team')), .funs = function(x) pfr_team_abbr[paste0(x)] %>% as.character) %>% 
  return
}

