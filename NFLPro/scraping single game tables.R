
## must acquire authorization token by logging into NFL Pro:
### 1) log into NFL pro and open any page where a table is populating
### 2) open up "Developer Tools" (F12 or Ctrl + Shift + I)
### 3) navigate to "Network" ribbon
### 4) find an XHR request type on the list of requests and click on it
### 5) scroll down to the "Request Headers" section
### 6) find the string to the right of "Authorization"
### 7) copy that string and paste as your token as a character string below
### the token is good for 60 minutes after being generated

token = ''

NFLPRO_single_game_table <- function(token, table_type = 'passing', week = 1, season = 2024) {
  
  ### take a two second break in between calls
  ### only need this step if you're running this function repeatedly
  Sys.sleep(2)
  
  ### vector to map week to appropriate text
  season_length = ifelse(season < 2021, 17, 18)
  week_slug_vec = c(paste0('WEEK_', 1:season_length), 'WC', 'DIV', 'CONF', 'SB')
  
  ### proper table url
  url_modifier = ifelse(table_type == 'defending', 'defense/overview', paste0('players-offense/', table_type))
  
  httr::GET(
    url = paste0('https://pro.nfl.com/api/stats/', url_modifier, '/week?season=', season, '&week=', week_slug_vec[week],'&limit=3997'),
    httr::add_headers(Authorization = token)
  ) |> 
  httr::content(as = 'parsed') |> 
  (function(i) i[[gsub('ing', 'ers', table_type)]])() |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(
    season = season,
    table_type = table_type,
    week = week
  )

}

### equipped to handle passing, rushing, receiving, and defending table_type
rush_df <- NFLPRO_single_game_table(
  token = token,
  table_type = 'rushing',
  week = 1,
  season = 2024
)

### some of the fields provided by NFL Pro can be joined to other public data
### here is how to join to the proper team, player, and game

teams_df <- nflreadr::load_teams()
roster_df <- nflreadr::load_rosters(2024)
sched_df <- nflreadr::load_schedules(2024) |> 
  dplyr::mutate(old_game_id = as.numeric(old_game_id))

rush_df |> 
  dplyr::left_join(teams_df, by = dplyr::join_by(teamId == team_id)) |> 
  dplyr::left_join(roster_df, by = dplyr::join_by(nflId == gsis_it_id)) |> 
  dplyr::left_join(sched_df, by = dplyr::join_by(gameId == old_game_id))
