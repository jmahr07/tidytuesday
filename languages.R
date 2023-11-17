library(tidyverse)

#DATA
tuesdata <- tidytuesdayR::tt_load("2023-03-21")
languages <- tuesdata$languages

opensource <- languages %>% 
  drop_na(is_open_source)

#PLOT
ggplot(opensource) + 
  geom_violin(aes(appeared, number_of_users, fill = is_open_source, color = is_open_source),
              trim = FALSE,
              orientation = "y", 
              alpha = 0.1,
              show.legend = FALSE) +
  geom_violin(aes(appeared, number_of_jobs, fill = is_open_source, color = is_open_source),
              trim = FALSE,
              orientation = "y",
              alpha = 0.4,
              linetype = 3,
              show.legend = FALSE) +
  geom_segment(aes(x = 1976, xend = 1976, y = 10000, yend = 2500), 
               color = "white",
               lineend = "round",
               arrow = arrow(length = unit(0.1, "in"))) +
  geom_segment(aes(x = 1976, xend = 1980, y = 10000, yend = 5000), 
               color = "white",
               lineend = "round",
               arrow = arrow(length = unit(0.1, "in"))) +
  geom_segment(aes(x = 1976, xend = 1980, y = 8, yend = 100), 
               color = "white",
               lineend = "round",
               arrow = arrow(length = unit(0.1, "in"))) +
  geom_segment(aes(x = 1976, xend = 1985, y = 8, yend = 100), 
               color = "white",
               lineend = "round",
               arrow = arrow(length = unit(0.1, "in"))) +
  geom_text(aes(1976, 12000, label = "Users"),
            hjust = 0.5,
            vjust = 0,
            color = "white",
            family = "Orator Std") +
  geom_text(aes(1976, 7, label = "Jobs"),
            hjust = 0.5,
            vjust = 1,
            color = "white",
            family = "Orator Std") +
  geom_text(aes(2011, 100000, label = "Open-Source"),
            color = "yellow",
            family = "Orator Std") +
  geom_text(aes(1990, 100000, label = "Not Open-Source"),
            color = "#F40076",
            family = "Orator Std") +
  scale_fill_manual(values = c("#F40076", "yellow")) +
  scale_color_manual(values = c("#F40076", "yellow")) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Programing Language Evolution",
       subtitle = "Number of users and jobs over time") +
  ylab("# of Users/Jobs") +
  theme_minimal() +
  theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "in"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white", family = "Orator Std"),
        axis.text = element_text(color = "darkgrey"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 25))

ggsave("languages.png", width = 10, height = 6, units = "in", dpi = 900) 
