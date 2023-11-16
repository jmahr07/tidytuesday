library(tidyverse)

# Data
tuesdata <- tidytuesdayR::tt_load('2023-02-21')
bob_ross <- tuesdata$bob_ross  

bob_ross <- bob_ross %>% 
  arrange(season, episode) %>% 
  mutate(n = 1:403)

colors <- bob_ross %>% 
  select("Black_Gesso":"Alizarin_Crimson") %>% 
  pivot_longer("Black_Gesso":"Alizarin_Crimson") %>% 
  distinct(name) %>% 
  separate(name, sep = "_", c("name1", "name2", "name3")) 
  
colors1 <- tibble(unite(colors, "full_name", name1:name3, na.rm = TRUE, sep = " "),
                  hex = c("#000000",
                        "#DB0000",
                        "#8A3324",
                        "#FFEC00",
                        "#5F2E1F",
                        "#CD5C5C",
                        "#FFB800",
                        "#000000",
                        "#FFFFFF",
                        "#000000",
                        "#0C0040",
                        "#102E3C",
                        "#021E44",
                        "#0A3410",
                        "#FFFFFF",
                        "#221B15",
                        "#C79B00",
                        "#4E1500"),
                  z = c(1,
                        7,
                        5,
                        10,
                        4,
                        6,
                        9,
                        1,
                        15,
                        1,
                        14,
                        12,
                        13,
                        11,
                        15,
                        2,
                        8,
                        3))

colors1 <- arrange(colors1, z)

#Plot
ggplot() +
  geom_rect(aes(xmin = -1.4, xmax = 19.5, ymin = -50, ymax = 412),
            fill = "transparent") +
  geom_segment(data = bob_ross,
               aes(x = 0, xend = 18, y = n, yend = n),
               color = "grey", size = 0.1) +
  geom_segment(data = subset(bob_ross, Black_Gesso == "TRUE"),
             aes(x = 0, xend =  1,
                 y = n, yend = n), 
             color = "#000000", size = 0.75) +
  geom_segment(data = subset(bob_ross, Liquid_Black == "TRUE"),
             aes(x = 1, xend =  2,
                 y = n, yend = n), 
             color = "#000000", size = 0.75) +
  geom_segment(data = subset(bob_ross, Midnight_Black == "TRUE"),
             aes(x = 2, xend =  3,
                 y = n, yend = n), 
             color = "#000000", size = 0.75) +
  geom_segment(data = subset(bob_ross, Van_Dyke_Brown == "TRUE"),
             aes(x = 3, xend =  4,
                 y = n, yend = n), 
             color = "#221B15", size = 0.75) +
  geom_segment(data = subset(bob_ross, Alizarin_Crimson == "TRUE"),
             aes(x = 4, xend =  5,
                 y = n, yend = n), 
             color = "#4E1500", size = 0.75) +
  geom_segment(data = subset(bob_ross, Dark_Sienna == "TRUE"),
             aes(x = 5, xend =  6,
                 y = n, yend = n), 
             color = "#5F2E1F", size = 0.75) +
  geom_segment(data = subset(bob_ross, Burnt_Umber == "TRUE"),
             aes(x = 6, xend =  7,
                 y = n, yend = n), 
             color = "#8A3324", size = 0.75) +
  geom_segment(data = subset(bob_ross, Indian_Red == "TRUE"),
             aes(x = 7, xend =  8,
                 y = n, yend = n), 
             color = "#CD5C5C", size = 0.75) +
  geom_segment(data = subset(bob_ross, Bright_Red == "TRUE"),
             aes(x = 8, xend = 9,
                 y = n, yend = n), 
             color = "#DB0000", size = 0.75) +
  geom_segment(data = subset(bob_ross, Yellow_Ochre == "TRUE"),
             aes(x = 9, xend = 10,
                 y = n, yend = n), 
             color = "#C79B00", size = 0.75) +
  geom_segment(data = subset(bob_ross, Indian_Yellow == "TRUE"),
             aes(x = 10, xend = 11,
                 y = n, yend = n), 
             color = "#FFB800", size = 0.75) +
  geom_segment(data = subset(bob_ross, Cadmium_Yellow == "TRUE"),
             aes(x = 11, xend = 12,
                 y = n, yend = n), 
             color = "#FFEC00", size = 0.75) +
  geom_segment(data = subset(bob_ross, Sap_Green == "TRUE"),
             aes(x = 12, xend = 13,
                 y = n, yend = n), 
             color = "#0A3410", size = 0.75) +
  geom_segment(data = subset(bob_ross, Phthalo_Green == "TRUE"),
             aes(x = 13, xend = 14,
                 y = n, yend = n), 
             color = "#102E3C", size = 0.75) +
  geom_segment(data = subset(bob_ross, Prussian_Blue == "TRUE"),
             aes(x = 14, xend = 15,
                 y = n, yend = n), 
             color = "#021E44", size = 0.75) +
  geom_segment(data = subset(bob_ross, Phthalo_Blue == "TRUE"),
             aes(x = 15, xend = 16,
                 y = n, yend = n), 
             color = "#0C0040", size = 0.75) +
  geom_segment(data = subset(bob_ross, Liquid_Clear == "TRUE"),
             aes(x = 16, xend = 17,
                 y = n, yend = n), 
             color = "#FFFFFF", size = 0.75) +
  geom_segment(data = subset(bob_ross, Titanium_White == "TRUE"),
             aes(x = 17, xend = 18,
                 y = n, yend = n), 
             color = "#FFFFFF", size = 0.75) +
  geom_text(data = bob_ross,
            aes(x = -0.1, y = n, label = paste("",painting_title,"|",painting_index,"")),
            hjust = 1, size = 0.5, family = "Avenir Next") +
  geom_text(data = colors1,
            aes(x = 0.5:17.5, y = -5, label = full_name),
            size = 3, family = "Avenir Next", fontface = "bold", angle = -90, vjust = 1, hjust = 0) +
  geom_text(data = bob_ross,
            aes(x = 18.1, y = n, label = paste("",season,"|",episode,"")),
            hjust = 0, size = 0.5, family = "Avenir Next") +
  geom_text(aes(x = -0.14, y = 407, label = "Name | Index #"),
            hjust = 0.5, size = 2, family = "Avenir Next", fontface = "bold") +
  geom_text(aes(x = 18.24, y = 407, label = "Season | Episode"),
            hjust = 0.5, size = 2, family = "Avenir Next", fontface = "bold") +
  labs(title = "Color Use in Bob Ross Paintings",
       subtitle = "In order of appearance on 'The Joy of Painting'\n with the most recent at the top",
       caption = "Data from Jared Wilber via @frankiethull Bob Ross Colors data package.") +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "in"),
        text = element_text(family = "Avenir Next"),
        plot.title = element_text(face= "bold", hjust = 0.5, vjust = -3, size = 20),
        plot.subtitle = element_text(hjust = 0.5, vjust = -5))

#Save
ggsave("bob_ross.png", height = 13, width = 8, units = "in", dpi = 900)


