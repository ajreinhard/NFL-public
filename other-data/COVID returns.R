setwd('C:/Users/Owner/Documents/GitHub/NFL/NFL')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

p <- tibble(
    days_on_list = factor(c(1:10,'>10'),c(1:10,'>10')),
    players_by_day = c(1,3,1,4,3,2,6,17,25,30,40)
  ) %>% 
  ggplot(aes(x = days_on_list, y = players_by_day)) + 
  geom_bar(stat = 'identity', fill = color_SB[1], color = 'darkblue', width = 0.7, size = 0.6) +
  geom_text(aes(label = players_by_day, y = players_by_day + 1.5), color = 'darkblue', family = font_SB, size = 3) +
  geom_segment(x = 0.6, xend = 7.4, y = 12, yend = 12, color = 'darkblue', size = 0.8, lineend = 'round') +
  geom_segment(x = 0.6, xend = 0.6, y = 11.2, yend = 12, color = 'darkblue', size = 0.8, lineend = 'round') +
  geom_segment(x = 7.4, xend = 7.4, y = 11.2, yend = 12, color = 'darkblue', size = 0.8, lineend = 'round') +
  geom_curve(x = 3.6, xend = 5.3, y = 23.75, yend = 12, color = 'darkblue', size = 0.8, curvature = -0.4) +
  geom_shadowtext(aes(x = 2.5, y = 26, label = 'Only 20 of the 132 players to test positive under\nthe original 2021 COVID protocols were able to be\nreactivated in seven days or less'), bg.r = 0.25, bg.color = 'white', family = font_SB, size = 2.5, color = 'darkblue') +
  labs(title = 'How fast have players been returning from the COVID-19 list?',
       subtitle = 'Number of players who have returned from a positive test between 7/31/21 to 12/16/21', 
       y = 'Player\nCount',
       x = 'Number of Days Spent on COVID-19 List'
       ) +
  scale_y_continuous(expand = c(0,0), limits = c(0,45), breaks = seq(0,45,5)) +
  scale_x_discrete(expand = expansion(add = c(0.9, 1.3))) +
  theme_SB + 
  theme(
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_blank()
  )

brand_plot(p, 'COVID time spent 21.png', asp = 16/9, fade_borders = 'tr', data_home = 'Data: @TBagleySports at Sharp Football Analysis', base_size = 4) 