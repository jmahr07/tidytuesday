library(tidyverse)

#Data
tuesdata <- tidytuesdayR::tt_load('2023-10-03')

grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

grants <- grants %>% filter(close_date <= '2030-12-31')

grants1 <- grants %>% select(opportunity_id, opportunity_number, opportunity_title, posted_date, close_date, estimated_funding)
grants1$duration <- grants1$close_date - grants1$posted_date
grants1$duration_no <- as.numeric(gsub("\\D", "", grants1$duration))
grants1$year <- format(grants1$posted_date, format="%Y")
grants_avg <- grants1 %>% group_by(year) %>% 
  summarize_at(vars(duration, estimated_funding), list(mean = mean), na.rm = TRUE)
grants_avg$estimated_funding_mean <- grants_avg$estimated_funding_mean/10000
grants_avg$funding <- sub("^", "$", formatC(grants_avg$estimated_funding_mean*10000, format = "d", big.mark = ","))

#Labels
years <- as.Date(c("2005-07-02","2006-07-02","2007-07-02","2008-07-02", "2009-07-02",
                   "2010-07-02","2011-07-02","2012-07-02","2013-07-02", "2014-07-02",
                   "2015-07-02","2016-07-02","2017-07-02","2018-07-02", "2019-07-02",
                   "2020-07-02","2021-07-02","2022-07-02","2023-07-02"),"%Y-%m-%d")

grants_label_y <- c(1685.0000,931.5500,769.9211,1141.3456,1987.2185,1131.4845,1359.1335,1081.3347,929.9012,
                 1011.1173,902.0563,916.4532,1067.3741,1591.6808,1559.5135,3478.7262,3155.1609,8215.6484,
                 3728.0800)
#Plot  
ggplot() +
  geom_segment(data = grants1, 
               aes(y = 0, yend = duration, x = posted_date, xend = posted_date),
               size = 0.02, color = "#44BBA4") +
  geom_point(data = subset(grants_avg, year == c(2005:2023)),
             aes(y = estimated_funding_mean, x = years),
             color = "#E7BB41", size = 3) +
  geom_line(data = subset(grants_avg, year == c(2005:2023)),
            aes(y = estimated_funding_mean, x = years),
            color = "#E7BB41", linetype = 2, size = 0.25) +
  geom_point(data = subset(grants_avg, year == c(2005:2023)),
             aes(y = duration_mean, x = years),
             color = "#E7E5DF", size = 2) +
  geom_line(data = subset(grants_avg, year == c(2005:2023)),
            aes(y = duration_mean, x = years),
            color = "#E7E5DF") +
  geom_text(aes(x = years, y = -300, label = c(2005:2023)),
            size = 2.5, hjust = 0.5, family = "Avenir Next", color = "#44BBA4") +
  geom_text(aes(x = as.Date("2004-11-01", "%Y-%m-%d"), y = c(1825,3650,5475,5840), label = c(5,10,15,"Duration (yrs)")),
            size = 2.5, hjust = 0.5, family = "Avenir Next", fontface = "bold", color = "#E7E5DF") +
  geom_point(aes(x = as.Date("2004-11-01", "%Y-%m-%d"), y = c(365,730,1095,1460,
                                                              2190,2555,2920,3285,
                                                              4015, 4380,4745,5110)),
             size = 0.5, color = "#E7E5DF") +
  geom_text(aes(x = years, y = grants_label_y + 400, label = grants_avg$funding),
            angle = 90, size = 3, hjust = 0, family = "Avenir Next", color = "#E7BB41") +
  labs(title = "U.S. Government Grant Opportunities",
       subtitle = "Avg. Grant Duration and Estimated Funding per Year",
       caption = "grants.gov | #tidytuesday") + 
  theme_void() +
  theme(plot.background = element_rect(fill = "#26292C"),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "in"),
        text = element_text(family = "Avenir Next", color = "#E7BB41"),
        plot.title = element_text(face= "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5))

#Save
ggsave("grants.png", height = 6, width = 11, units = "in", dpi = 900)
