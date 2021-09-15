library(nflreadr)
library(fastrmodels)
library(tidyverse)
library(scales)
library(shadowtext)
library(ggridges)

# need these helpers from nflfastR repo
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_nflscrapr_mutations.R')
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_xyac.R')

# get initial pbp
pbp <- nflreadr::load_pbp(2021) %>%
  dplyr::select(-tidyselect::any_of(drop.cols.xyac)) %>% 
  dplyr::mutate(index = 1:dplyr::n())

# prepare_xyac_data helper function shown below
passes <- prepare_xyac_data(pbp) %>%
  dplyr::filter(.data$valid_pass == 1)


join_data <- passes %>%
  dplyr::select(
    "index", "distance_to_goal", "season", "week", "home_team", "posteam", "roof",
    "half_seconds_remaining", "down", "ydstogo",
    "posteam_timeouts_remaining", "defteam_timeouts_remaining",
    "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
  ) %>%
  dplyr::mutate(
    down = as.integer(.data$down),
    ydstogo = as.integer(.data$ydstogo),
    original_ydstogo = .data$ydstogo
  ) %>%
  dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())

xyac_vars <-
  stats::predict(
    fastrmodels::xyac_model,
    as.matrix(passes %>% xyac_model_select())
  ) %>%
  tibble::as_tibble() %>% 
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(
    tibble::tibble(
      "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
      "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
    ) %>%
      dplyr::left_join(join_data, by = "index") %>%
      dplyr::mutate(
        half_seconds_remaining = dplyr::if_else(
          .data$half_seconds_remaining <= 6,
          0,
          .data$half_seconds_remaining - 6
        )
      )
  ) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(
    max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
    max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
    cum_prob = cumsum(.data$prob),
    prob = dplyr::case_when(
      # truncate probs at loss greater than max loss
      .data$yac == .data$max_loss ~ .data$cum_prob,
      # same for gains bigger than possible
      .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
      TRUE ~ .data$prob
    ),
    # get end result for each possibility
    yardline_100 = .data$distance_to_goal - .data$yac
  ) %>%
  dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::mutate(
    posteam_timeouts_pre = .data$posteam_timeouts_remaining,
    defeam_timeouts_pre = .data$defteam_timeouts_remaining,
    gain = .data$original_spot - .data$yardline_100,
    turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
    down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
    ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
    # possession change if 4th down failed
    down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
    ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
    # flip yardline_100 and timeouts for turnovers
    yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
    posteam_timeouts_remaining = dplyr::if_else(
      .data$turnover == 1,
      .data$defeam_timeouts_pre,
      .data$posteam_timeouts_pre
    ),
    defteam_timeouts_remaining = dplyr::if_else(
      .data$turnover == 1,
      .data$posteam_timeouts_pre,
      .data$defeam_timeouts_pre
    ),
    # ydstogo can't be bigger than yardline
    ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
  ) %>%
  dplyr::ungroup() %>%
  nflfastR::calculate_expected_points() %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(
    ep = dplyr::case_when(
      .data$yardline_100 == 0 ~ 7,
      .data$turnover == 1 ~ -1 * .data$ep,
      TRUE ~ ep
    ),
    epa = .data$ep - .data$original_ep,
    wt_epa = .data$epa * .data$prob,
    wt_yardln = .data$yardline_100 * .data$prob,
    med = dplyr::if_else(
      cumsum(.data$prob) > .5 & dplyr::lag(cumsum(.data$prob) < .5), .data$yac, as.integer(0)
    )
  )

# rosters so we can eliminate non-WRs & TEs
roster_df <- nflreadr::load_rosters(2021)

# range of oucomes on a catch
catch_outcomes_df <- passes %>% 
  select(season, game_id, play_id, index, gsis_id = receiver_id, cp, complete_pass, actual_yards_gained = yards_gained) %>% 
  left_join(roster_df) %>% 
  left_join(xyac_vars) %>% 
  filter(position %in% c('WR', 'TE')) %>% 
  rename(yac_prob = prob) %>% 
  mutate(
    PPR_points = 1 + gain/10 + ifelse(gain == original_spot, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained == gain & complete_pass == 1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    target = 0,
    game_played = 0
  )

# outcome for an incompletion  
incomplete_outcomes_df <- catch_outcomes_df %>% 
  group_by(index) %>% 
  filter(row_number() == 1) %>% 
  ungroup %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    target = 1
  ) %>%
  group_by(game_id, gsis_id) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup

# get the top 4 WRs on each team
WR_rank_df <- bind_rows(catch_outcomes_df, incomplete_outcomes_df) %>% 
  group_by(team, gsis_id) %>% 
  summarize(
    tot_PPR = sum(actual_PPR_points, na.rm = T),
    tot_targ = sum(target),
    tot_gp = sum(game_played),
    PPR_pg = tot_PPR / tot_gp
  ) %>% 
  arrange(-tot_targ) %>% 
  mutate(tm_rnk = row_number()) %>% 
  filter(tm_rnk <= 4)


# make a data frame to loop around
sampling_df <- bind_rows(catch_outcomes_df, incomplete_outcomes_df) %>% 
  right_join(WR_rank_df %>% select(team, gsis_id)) %>% 
  select(season, game_id, play_id, team, gsis_id, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)

# do sim
sim_df <- do.call(rbind, lapply(1:10000, function(x) {
  sampling_df %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, game_id, play_id, team, gsis_id, sim_res) %>% 
    distinct %>% 
    group_by(team, gsis_id) %>% 
    summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))

sim_df <- sim_df %>% mutate(sim = 1)

# calculate how many points were actually scored
actual_df <- catch_outcomes_df %>%
  group_by(team, gsis_id) %>% 
  summarize(sim_tot = sum(actual_PPR_points, na.rm = T), .groups = 'drop') %>% 
  mutate(sim = 0)

# figure out what percentile the actual values fall in
percentile_df <- rbind(sim_df, actual_df) %>% 
  group_by(team, gsis_id) %>% 
  mutate(perc = percent_rank(sim_tot)) %>% 
  filter(sim == 0) %>% 
  mutate(sim_tot = NULL, sim = NULL)

# make the viz
sim_df %>% 
  left_join(percentile_df) %>% 
  left_join(WR_rank_df) %>% 
  left_join(roster_df) %>% 
  mutate(
    sim_pg = sim_tot / tot_gp,
    pl_lab = paste0(full_name, '\n', number(perc * 100, accuracy = 0.1), ' perc.')
  ) %>% 
  group_by(team, gsis_id) %>% 
  mutate(obs_num = row_number()) %>% 
  ungroup %>% 
  ggplot(aes(x = sim_pg, y = tm_rnk, group = gsis_id, label = pl_lab)) +
  facet_wrap(. ~ team, nrow = 4, scales = 'free') +
  geom_point(aes(x = ifelse(obs_num==1, PPR_pg, NA), y = tm_rnk + 0.08), color = 'darkblue', fill = '#ff7f00', shape = 24, size = 0.6, stroke = 0.2, na.rm = T) +
  ggridges::stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1,.25,.75,.9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 0.9,
    color = 'grey50',
    size = 0.2,
    show.legend = F
  ) +
  geom_shadowtext(aes(x = ifelse(obs_num==1, 47, NA), y = tm_rnk - 0.5), hjust = 1, color = 'darkblue', bg.color = 'white', size = 1.2, na.rm = T, bg.r = 0.2) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(mult = 0), limits = c(0,50)) +
  scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1,0)) +
  scale_fill_manual(values = c(alpha('#ff7f00', 0.4),alpha('#00008b', 0.4),alpha('grey60', 0.4),alpha('#00008b', 0.4),alpha('#ff7f00', 0.4))) +
  labs(title = paste0('2021 Regular Season Expected PPR Fantasy Points per Game, Week 1'),
       subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
       x = NULL) +
  theme_classic() + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 4),
    axis.ticks.length = unit(0.15, 'lines'),
    panel.grid.minor.x = element_line('grey95', size = 0.2),
    panel.spacing.x = unit(0.5, 'lines')
  )

