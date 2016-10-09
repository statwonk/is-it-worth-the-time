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
         how_much_time_you_save_in_five_years = how_much_time_you_save_in_one_day*365.25*5 / (60*60*24)) %>%
  mutate(how_often_you_do_the_task = factor(how_often_you_do_the_task, levels = unique(how_often_you_do_the_task)),
         how_much_time_you_shave_off = factor(how_much_time_you_shave_off, levels = rev(unique(how_much_time_you_shave_off)))) %>%
  mutate(how_much_time_you_save_in_five_years = ifelse(
    how_much_time_you_save_in_five_years > 365.25*5,
    NA, how_much_time_you_save_in_five_years
  )) %>%
  ggplot(aes(x = how_often_you_do_the_task,
             y = how_much_time_you_shave_off
  )) +
  geom_tile(fill = "white", colour = "black") +
  geom_text(aes(
    label = how_much_time_you_save_in_five_years)
  ) +
  scale_x_discrete(
    position = "top",
    labels = c(
      "50x per day",
      "5x per day",
      "Daily",
      "Weekly",
      "Monthly",
      "Yearly"
    )
  ) +
  scale_y_discrete(
    position = "left",
    labels = c(
      "1 second",
      "5 seconds",
      "30 seconds",
      "1 minute",
      "5 minutes",
      "30 minutes",
      "1 hour",
      "6 hours",
      "1 day"
    ) %>% rev) +
  ggtitle("How much time will you save with an automation?") +
  xlab("How often do you do the task?") +
  ylab("How long does the task take?") +
  theme_xkcd() +
  theme(legend.position = "none",
        panel.grid.minor = element_line(colour = "black"))