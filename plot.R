library(ggplot2)
library(xkcd)
library(dplyr)

expand.grid(
  how_much_time_you_shave_off = c(
    1, # seconds
    1*5,
    1*30,
    1*60,
    1*60*5,
    1*60*30,
    1*60*60,
    1*60*60*6,
    1*60*60*24 # seconds
  ),
  how_often_you_do_the_task = c(
    50, # times per day
    5,
    1,
    1/7,
    1/(365.25/12),
    1/365.25 # times per day
  )
) %>% tbl_df() %>%
  mutate(how_much_time_you_save_in_one_day = how_often_you_do_the_task * how_much_time_you_shave_off,
         how_much_time_you_save_in_one_year = how_much_time_you_save_in_one_day*365.25) %>%
  mutate(how_often_you_do_the_task = factor(how_often_you_do_the_task, levels = unique(how_often_you_do_the_task)),
         how_much_time_you_shave_off = factor(how_much_time_you_shave_off, levels = unique(how_much_time_you_shave_off))) %>%
  ggplot(aes(x = how_often_you_do_the_task,
             y = how_much_time_you_shave_off
  )) +
  geom_tile(fill = "white", colour = "black") +
  geom_text(aes(
    label = how_much_time_you_save_in_one_year)
  ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "left") +
  theme_xkcd() +
  theme(legend.position = "none",
        panel.grid.minor = element_line(colour = "black"))