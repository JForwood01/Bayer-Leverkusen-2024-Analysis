pacman::p_load(tidyverse, janitor, StatsBombR, kableExtra, webshot2, grid, install = TRUE)

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
       caption = "\nPlot created by James Forwood / GitHub: JForwood01") +
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
        legend.position = "right") +
  coord_flip() +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 55, xmax = 60,
                    ymin = 0, ymax = 20)

ggsave(filename = paste0(getwd(), "/Plots/shot_plot.jpg"),
       plot = shot_plot,
       device = "jpeg",
       width = 13,
       height = 10)

# Assist Locations

## Cleaning data for passes
key_passes <- shots %>%
  drop_na(shot_key_pass_id)

key_passes <- key_passes$shot_key_pass_id

pass <- events %>%
  filter(type_name == "Pass" & id %in% key_passes & play_pattern_name == "Regular Play") %>%
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

p_s_plot <- pass_and_shot %>%
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
       caption = "\nPlot created by James Forwood / GitHub: JForwood01") +
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
        legend.position = "right") +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 10, xmax = 40,
                    ymin = -30, ymax = 40) 

ggsave(filename = paste0(getwd(), "/Plots/pass_shot_plot.jpg"),
       plot = p_s_plot,
       device = "jpeg",
       width = 13,
       height = 10)

# Analyse key passers
passers <- pass %>%
  filter(team_name == "Bayer Leverkusen") %>%
  group_by(player_name) %>%
  count(player_name) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  separate(player_name, into = c("Name 1", "Name 2", "Name 3"), sep = " ", remove = FALSE) %>%
  mutate("surname" = if_else(is.na(`Name 3`), paste(`Name 2`), paste(`Name 3`))) %>%
  slice(c(1:12))

passers_plot <- passers %>%
  mutate(surname = if_else(`Name 2` == "Grimaldo", paste("Grimaldo"), surname)) %>%
  ggplot(aes(x = reorder(surname, -n), y = n)) +
  geom_bar(stat = "identity",
           fill = "#E32221", # this is the Bayer Leverkusen Red HEX Code
           colour = "black") +
  geom_text(aes(label = n),
            stat = "identity", 
            position = "identity",
            vjust = -0.5) +
  labs(y = "Number of Key Passes Completed",
       x = "Player Surname",
       title = "Bayer Leverkusen Opportunity Creators",
       subtitle = "Data from 2023/24 Bundesliga Season. Provided by StatsBomb.",
       caption = "GitHub: JForwood01") +
  ggpubr::theme_pubclean() +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        plot.caption = element_text(size = 10,
                                    hjust = 1),
        legend.position = "right") +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 10, xmax = 12,
                    ymin = 25, ymax = 30) 

ggsave(filename = paste0(getwd(), "/Plots/passers_plot.jpg"),
       plot = passers_plot,
       device = "jpeg",
       width = 13,
       height = 10)

passers_list <- passers %>%
  slice(c(1:5)) 

passers_list <- passers_list$player_name

# Analyse heatmaps of key players

gps_data <- StatsBombR::free_allevents_360(MatchesDF = matches)


gps_data_joined <- left_join(events, gps_data, by = join_by(id == event_uuid))
#tidying gps data

gps_tidy <- gps_data_joined %>%
  unnest(freeze_frame) %>%
  filter(actor == TRUE & player_name %in% passers_list) %>%
  select(c(id, player_name, actor, location)) %>%
  unnest_wider(location, names_sep = "_") %>%
  rename("gps_x" = "location_1",
         "gps_y" = "location_2")

gps_tidy$player_name = factor(gps_tidy$player_name, levels = c("Florian Wirtz",
                                                               "Jeremie Frimpong",
                                                               "Jonas Hofmann",
                                                               "Granit Xhaka",
                                                               "Alejandro Grimaldo García"))
#events_gps_tidy <- events_gps %>%
# select(c(id, player_name, freeze_frame, visible_area, pass_end_x_coord, pass_end_y_coord, x_coord, y_coord)) %>%
# filter((player_name %in% passers_list)) %>%
# unnest(freeze_frame) %>%
# filter(actor == TRUE) %>%
#unnest_wider(location, names_sep = "_")

filtered_pass <- pass_and_shot %>%
  filter(player_name %in% passers_list)

filtered_pass$player_name = factor(filtered_pass$player_name, levels = c("Florian Wirtz",
                                                                         "Jeremie Frimpong",
                                                                         "Jonas Hofmann",
                                                                         "Granit Xhaka",
                                                                         "Alejandro Grimaldo García"))

gps_plot <- ggplot(gps_tidy, aes(x = gps_x, y = gps_y)) +
  StatsBombR::annotate_pitchSB() +
  stat_density_2d(geom = "polygon",
                  aes(alpha = after_stat(level),
                      fill = after_stat(level)))  +
  scale_fill_viridis_c(na.value = "transparent",
                       option = "plasma")  +
  scale_alpha_continuous(range = c(0.1, 0.7)) +
  geom_segment(data = filtered_pass,
               aes(x = x_coord_pass, y = y_coord_pass, xend = x_coord_shot, yend = y_coord_shot),
               linewidth = 0.8,
               alpha = 0.5,
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) +
  labs(x = " ",
       y = " ",
       title = "Bayer Leverkusen Opportunity Creators",
       subtitle = "Data from 2023/24 Bundesliga Season. Provided by StatsBomb.",
       caption = "\nPlot created by James Forwood / GitHub: JForwood01") +
  xlim(c(0, 120)) +
  ylim(c(0,80)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        plot.caption = element_text(size = 10,
                                    hjust = 1)) + 
  facet_wrap(~ player_name) +
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 0, xmax = 30,
                    ymin = 0, ymax = 10)

ggsave(filename = paste0(getwd(), "/Plots/gps.jpg"),
       plot = gps_plot,
       device = "jpeg",
       width = 13,
       height = 10)

## Neural nodes outside the box.
node_data <- events %>%
  filter(team_name == "Bayer Leverkusen" & id %in% filtered_pass$id & player_name == "Granit Xhaka") %>%
  select(c(player_name, id, type_name, x_coord, y_coord, pass_end_location, pass_recipient_name)) %>%
  drop_na(x_coord) %>%
  unnest_wider(pass_end_location, names_sep = "_") %>%
  group_by(player_name) %>%
  mutate("passer_mean_x" = mean(x_coord),
         "passer_mean_y" = mean(y_coord)) %>%
  ungroup() %>%
  group_by(player_name, pass_recipient_name) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(-c(id, type_name, x_coord, y_coord)) %>%
  distinct()

xhaka_nodes <- node_data %>%
  filter(n > 1 & pass_end_location_1 > passer_mean_x) %>%
  mutate("wirtz_passes" = sum(n)) %>%
  ggplot() +
  StatsBombR::annotate_pitchSB() +
  geom_segment(aes(x = passer_mean_x, y = passer_mean_y, 
                   xend = pass_end_location_1, yend = pass_end_location_2),
               colour = "darkgrey") +
  # add player 1 nodes
  geom_point(aes(x = passer_mean_x, y = passer_mean_y)) +
  # add player 2 nodes
  geom_point(aes(x = pass_end_location_1, y = pass_end_location_2, 
                 colour = factor(pass_recipient_name)),
             size = 2) +
  scale_size_continuous(guide = guide_none()) +
  labs(x = " ",
       y = " ",
       title = "Granit Xhaka's Targets for Key Passes",
       subtitle = "Data from 2023/24 Bundesliga Season. Provided by StatsBomb.",
       caption = "\nPlot by James Forwood / GitHub: JForwood01",
       colour = "Pass Recipient") +
  xlim(c(0, 120)) +
  ylim(c(0,80)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        plot.caption = element_text(size = 10,
                                    hjust = 1),
        legend.position = "right") + 
  annotation_custom(rasterGrob(StatsBomb_logo),
                    xmin = 0, xmax = 30,
                    ymin = 0, ymax = 10)

ggsave(filename = paste0(getwd(), "/Plots/Xhaka.jpg"),
       plot = xhaka_nodes,
       device = "jpeg",
       width = 13,
       height = 10)
