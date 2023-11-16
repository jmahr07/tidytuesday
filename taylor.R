library(tidyverse)
library(taylor)
library(forcats)

# Data (included in {taylor} package)
album_songs <- taylor_album_songs
songs <- taylor_all_songs
albums <- taylor_albums

album_songs$number <- 1:194

# Unnest data and searh lyrics for identifiers
album_songs_unnested <- unnest(album_songs, cols = c(lyrics))
album_songs_unnested$he <- str_detect(album_songs_unnested$lyric, 
                                      "\\bHe\\b|\\bhe\\b|\\bHim\\b|\\bhim\\b|\\bHis\\b|\\bhis\\b|\\bBoy\\b|\\bboy\\b|\\bMan\\b|\\bman\\b")
album_songs_unnested$she <- str_detect(album_songs_unnested$lyric, 
                                       "\\bShe\\b|\\bshe\\b|\\bHer\\b|\\bher\\b|\\bHers\\b|\\bhers\\b|\\bGirl\\b|\\bgirl\\b|\\bWoman\\b|\\bwoman\\b")
album_songs_unnested$i <- str_detect(album_songs_unnested$lyric, 
                                     "\\bI\\b|\\bMe\\b|\\bme\\b|\\bMy\\b|\\bmy\\b")
album_songs_unnested$you <- str_detect(album_songs_unnested$lyric, 
                                       "\\bYou\\b|\\byou\\b|\\bYour\\b|\\byour\\b|\\bYours\\b|\\byours\\b")
album_songs_unnested$we <- str_detect(album_songs_unnested$lyric, 
                                      "\\bWe\\b|\\bwe\\b|\\bOur\\b|\\bour\\b|\\bOurs\\b|\\bours\\b")
album_songs_unnested$them <- str_detect(album_songs_unnested$lyric, 
                                        "\\bThey\\b|\\bthey\\b|\\bThem\\b|\\bthem\\b|\\bTheir\\b|\\their\\b|\\bTheirs\\b|\\theirs\\b")

##Pronoun Count Per Song
album_order <- c("Taylor Swift",
                 "Fearless (Taylor's Version)",
                 "Speak Now",
                 "Red (Taylor's Version)",
                 "1989",
                 "reputation",
                 "Lover",
                 "folklore",
                 "evermore",
                 "Midnights")

pronoun_count <- album_songs_unnested %>% 
  select(number, album_release, track_number, album_name, track_name, lyric, he, she, i, you, we, them) %>% 
  group_by(number, album_release, album_name, track_number, track_name) %>% 
  summarize(hecount = sum(he), shecount = sum(she), icount = sum(i), youcount = sum(you), wecount = sum(we), themcount = sum(them))

pronoun_totals <- pronoun_count %>%
  group_by(album_name) %>% 
  summarize(he = sum(hecount), she = sum(shecount), i = sum(icount),  you = sum(youcount), we = sum(wecount), them = sum(themcount)) %>% 
  slice(match(album_order, album_name))

full_totals <- tibble(he = sum(pronoun_totals$he), 
                      she = sum(pronoun_totals$she), 
                      i = sum(pronoun_totals$i), 
                      you = sum(pronoun_totals$you), 
                      we = sum(pronoun_totals$we), 
                      them = sum(pronoun_totals$them))

track_total <- album_songs %>% select(album_name, track_name) %>% 
  group_by(album_name) %>% 
  summarize(count = n()) %>% 
  slice(match(album_order, album_name))

##aes
xmin <- rep(0.5:193.5, times = 6)
xmax <- rep(1.5:194.5, times = 6)
ymin <- c(rep(0, times = 194),
          rep(75, times = 194),
          rep(150, times = 194),
          rep(225, times = 194),
          rep(300, times = 194),
          rep(375, times = 194))
ymax <- c(pronoun_count$icount,
          pronoun_count$youcount + 75,
          pronoun_count$wecount + 150,
          pronoun_count$themcount + 225,
          pronoun_count$hecount + 300,
          pronoun_count$shecount + 375)

rect_fill <- c(rep("#43475b", times = 194),
               rep("#CE986B", times = 194),
               rep("#2F3C30", times = 194),
               rep("#0B0B08", times = 194),
               rep("#B98645", times = 194),
               rep("#8D8C7A", times = 194))

#Plot
ggplot() +
  
  ##bars
  geom_rect(aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = rect_fill) +
  geom_text(aes(x = rep(1:194, times = 6),
                y = ymax + 1,
                label = c(pronoun_count$icount,
                          pronoun_count$youcount,
                          pronoun_count$wecount,
                          pronoun_count$themcount,
                          pronoun_count$hecount,
                          pronoun_count$shecount)),
            size = 1,
            vjust = 0,
            family = "Avenir Next") +
  
  ##track names
  geom_text(aes(x = 1:194,
                y = -1,
                label = pronoun_count$track_name),
            size = 1, angle = -90, hjust = 0, family = "Avenir Next") +
  
  ##album lines and names
  geom_segment(aes(x = c(15.5, 41.5, 58.5, 88.5, 104.5, 119.5, 137.5, 154.5, 171.5),
                   xend = c(15.5, 41.5, 58.5, 88.5, 104.5, 119.5, 137.5, 154.5, 171.5),
                   y = -100,
                   yend = 450),
               size = 0.15,
               color = "#0B0B08",
               alpha = 0.75) +
  geom_text(aes(x = c(7.5, 28.5, 49.5, 73.5, 96.5, 111.5, 128.5, 145.5, 162.5, 182.5),
                y = 450,
                label = c("Taylor Swift", 
                          "Fearless (Taylor's Version)", 
                          "Speak Now",
                          "Red (Taylor's Version)",
                          "1989",
                          "reputation",
                          "Lover",
                          "folklore",
                          "evermore",
                          "Midnights")),
            size = 2.5,
            color = "#0B0B08",
            family = "Avenir Next",
            fontface = "bold") +
  
  ##averages and totals
  geom_text(aes(x = rep(c(9.5, 35.5, 52.5, 82.5, 98.5, 113.5, 131.5, 148.5, 165.5, 188.5), times = 6),
                y = c(rep(60, times = 10),
                      rep(135, times = 10),
                      rep(210, times = 10),
                      rep(285, times = 10),
                      rep(360, times = 10),
                      rep(435, times = 10)),
                label = c(pronoun_totals$i,
                          pronoun_totals$you,
                          pronoun_totals$we,
                          pronoun_totals$them,
                          pronoun_totals$he,
                          pronoun_totals$she)),
            size = 2,
            hjust = 0,
            family = "Avenir Next",
            color = "#0B0B08",
            fontface = "bold") +
  geom_text(aes(x = rep(c(9.5, 35.5, 52.5, 82.5, 98.5, 113.5, 131.5, 148.5, 165.5, 188.5), times = 6),
                y = c(rep(60-6.5, times = 10),
                      rep(135-6.5, times = 10),
                      rep(210-6.5, times = 10),
                      rep(285-6.5, times = 10),
                      rep(360-6.5, times = 10),
                      rep(435-6.5, times = 10)),
                label = c(round(pronoun_totals$i/track_total$count, digits = 0),
                          round(pronoun_totals$you/track_total$count, digits = 0),
                          round(pronoun_totals$we/track_total$count, digits = 0),
                          round(pronoun_totals$them/track_total$count, digits = 0),
                          round(pronoun_totals$he/track_total$count, digits = 0),
                          round(pronoun_totals$she/track_total$count, digits = 0))),
            size = 1.75,
            hjust = 0,
            family = "Avenir Next",
            color = "#0B0B08",
            fontface = "italic") +
  geom_text(aes(x = 195,
                y = c(42.5, 117.5, 192.5, 267.5, 342.5, 417.5),
                label = c(full_totals$i,
                          full_totals$you,
                          full_totals$we,
                          full_totals$them,
                          full_totals$he,
                          full_totals$she)),
            size = 3,
            hjust = 0,
            family = "Avenir Next",
            fontface = "bold",
            color = "#0B0B08") +
  geom_text(aes(x = 195,
                y = c(32.5, 107.5, 182.5, 257.5, 332.5, 407.5),
                label = c(round(full_totals$i/194, digits = 0),
                          round(full_totals$you/194, digits = 0),
                          round(full_totals$we/194, digits = 0),
                          round(full_totals$them/194, digits = 0),
                          round(full_totals$he/194, digits = 0),
                          round(full_totals$she/194, digits = 0))),
            size = 2.5,
            hjust = 0,
            family = "Avenir Next",
            fontface = "italic",
            color = "#0B0B08") +
  geom_text(aes(x = -1,
                y = c(37.5, 112.5, 187.5, 262.5, 337.5, 412.5),
                label = c("Herself", "You", "Us", "Them", "He", "She")),
            size = 3,
            hjust = 1,
            family = "Avenir Next",
            fontface = "bold",
            color = "#0B0B08") +
  geom_text(aes(x = 189,
                y = c(-100, -118),
                label = c("Total Count", "Average\n(per song)")),
            size = 3,
            hjust = 0,
            family = "Avenir Next",
            fontface = c("bold","italic"),
            color = "#0B0B08",
            lineheight = 0.8) +
  
  ##labels
  geom_label(aes(x = 90,
                 y = -70,
                 label = "Each column indicates the number of uses of both personal and possesive pronouns (e.g., \"He\", \"Him\", and \"His\") within\nthe lyrics of each song, grouped by subject/object. Additionally, \"Man\", \"Boy\", \"Woman\", and \"Girl\" were counted for in\nthe \"He\" and \"She\" groups, respectively. Totals and averages are given for each identifier group and each album.\n\nTaylor most often references herself, and sings about \"you\" nearly as frequently. Group identifiers (\"us\", \"them\") are more\nrare, and gendered identifiers are among the least used."),
             size = 2.5,
             hjust = 0,
             vjust = 1,
             family = "Avenir Next",
             color = "#2F3C30",
             label.padding = unit(0.5, "lines")) +
  
  ##theme
  ylim(-150, 450) +
  theme_void() +
  labs(title =  "\"You\" Belong(s) With \"Me\" | Who is Taylor Singing About?",
       subtitle = "The use of identifiers in Taylor Swift's lyrics",
       caption = "Thompson W (2023). taylor: Lyrics and Song Data for Taylor Swift's Discography. https://taylor.wjakethompson.com, https://github.com/wjakethompson/taylor.") +
  theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "in"),
        text = element_text(family = "Avenir Next", color = "#0B0B08"),
        plot.title = element_text(face= "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        plot.background = element_rect(fill = "#F3F4F6",
                                       color = "#F3F4F6"))

#Save
ggsave("taylor.png", height = 9, width = 13, units = "in", dpi = 900)