pacman::p_load(tidyverse, janitor, StatsBombR, grid,  install = TRUE)

StatsBomb_logo = png::readPNG(source = paste0(getwd(), "/StatsBomb Logo/1. Colour Positive/SB - Icon Lockup - Colour positive.png"))
#load data
comps <- StatsBombR::FreeCompetitions()
comps <- comps %>%
  filter(competition_id == 9 & season_id == 281)

matches <- StatsBombR::FreeMatches(comps)

events <- StatsBombR::free_allevents(MatchesDF = matches)

events <- events %>%
  janitor::clean_names()

# examine shot locatons
shots <- events %>%
  filter(type_name == "Shot") %>%
  unnest_wider(location, names_sep = "_") %>%
  rename("x_coord" = "location_1",
         "y_coord" = "location_2") %>%
  mutate("shot_goal_outcome" = if_else(shot_outcome_name == "Goal", "Goal", "No Goal"))

shot_plot <- shots %>%
  filter(!(shot_type_name == "Penalty") & play_pattern_name == "Regular Play") %>%
  ggplot() +
  StatsBombR::annotate_pitchSB() +
  geom_point(aes(x = x_coord,
                 y = y_coord,
                 size = shot_statsbomb_xg,
                 colour = shot_goal_outcome,
                 alpha = shot_goal_outcome)) +
  scale_alpha_manual(values = c("Goal" = 0.7, "No Goal" = 0.25), guide = "none") +
  xlim(c(58, 120)) +
  labs(size = "xG",
       colour = "Outcome",
       x = " ",
       y = " ",
       title = "Bayer Leverkusen Open Play Shots",
       subtitle = "Data from 2023/24 Bundesliga Season",
       caption = "Data provided by StatsBomb") +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        plot.caption = element_text(vjust = 10,
                                    size = 10,
                                    hjust = 0.07)) +
  coord_flip() +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 55, xmax = 60,
                    ymin = 0, ymax = 20)

ggsave(filename = paste0(getwd(), "/Plots/shot_plot.jpg"),
       plot = shot_plot,
       device = "jpeg")


