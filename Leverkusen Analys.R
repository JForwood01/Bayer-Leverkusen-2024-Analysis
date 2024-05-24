pacman::p_load(tidyverse, janitor, StatsBombR, grid,  install = TRUE)

StatsBomb_logo = png::readPNG(source = paste0(getwd(), "/StatsBomb Logo/1. Colour Positive/SB - Icon Lockup - Colour positive.png"))
#load data
comps <- StatsBombR::FreeCompetitions()
comps <- comps %>%
  filter(competition_id == 9 & season_id == 281)

matches <- StatsBombR::FreeMatches(comps)

events <- StatsBombR::free_allevents(MatchesDF = matches)

events <- events %>%
  janitor::clean_names() %>%
  unnest_wider(location, names_sep = "_") %>%
  rename("x_coord" = "location_1",
         "y_coord" = "location_2") %>%
  mutate("shot_goal_outcome" = if_else(shot_outcome_name == "Goal", "Goal", "No Goal"))

# examine shot locatons
shots <- events %>%
  filter(type_name == "Shot") %>%
  select(c(shot_type_name, play_pattern_name, x_coord, y_coord, shot_goal_outcome, shot_statsbomb_xg,
           shot_key_pass_id))

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
       subtitle = "Data from 2023/24 Bundesliga Season. Provided by StatsBomb.",
       caption = "Image created by James Forwood \nGitHub: JForwood01") +
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
                                    hjust = 0.94)) +
  coord_flip() +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 55, xmax = 60,
                    ymin = 0, ymax = 20)

ggsave(filename = paste0(getwd(), "/Plots/shot_plot.jpg"),
       plot = shot_plot,
       device = "jpeg")

# Assist Locations

## Cleaning data for passes
key_passes <- shots %>%
  drop_na(shot_key_pass_id)

key_passes <- key_passes$shot_key_pass_id

pass <- events %>%
  filter(type_name == "Pass" & id %in% key_passes) %>%
  select(c(id, team_name, player_name, pass_recipient_name, x_coord, y_coord, pass_end_location, pass_type_name, pass_outcome_name)) %>%
  drop_na(player_name) %>%
  unnest_wider(pass_end_location, names_sep = "_") %>%
  rename("pass_end_x_coord" = "pass_end_location_1",
         "pass_end_y_coord" = "pass_end_location_2") %>%
  replace_na(replace = list(pass_type_name = "Regular", pass_outcome_name = "Complete")) %>%
  # just keep passes that were incomplete or complete
  filter(pass_outcome_name %in% c("Incomplete", "Complete"))

pass_and_shot <- left_join(pass, shots, by = join_by(id == shot_key_pass_id), suffix = c("_pass", "_shot"))
## Getting passes into the width of the box

pass_and_shot %>%
  filter(!(shot_type_name == "Penalty") & play_pattern_name == "Regular Play") %>%
  ggplot() +
  StatsBombR::annotate_pitchSB() +
  geom_segment(aes(x = x_coord_pass, y = y_coord_pass, xend = x_coord_shot, yend = y_coord_shot,
                   colour = shot_goal_outcome,
                   alpha = shot_statsbomb_xg,
                   linewidth = shot_statsbomb_xg),
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  scale_linewidth_continuous(range = c(0.1, 1)) +
  scale_alpha_continuous(range = c(0.15, 1)) +
  labs(size = "xG",
       colour = "Outcome",
       linewidth = "Shot xG",
       x = " ",
       y = " ",
       alpha = "Shot xG",
       title = "Bayer Leverkusen Key Pass Locations",
       subtitle = "Data from 2023/24 Bundesliga Season. Provided by StatsBomb.",
       caption = "Image created by James Forwood \nGitHub: JForwood01") +
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
                                    hjust = 0.94),
        legend.position = "top") +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 10, xmax = 40,
                    ymin = -30, ymax = 40) 

ggsave(filename = paste0(getwd(), "/Plots/pass_shot_plot.jpg"),
       plot = p_s_plot,
       device = "jpeg")
