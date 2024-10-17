# Load necessary libraries
library(cfbfastR)
library(tidyverse)
library(ggtext)
library(magick)
library(cowplot)
library(ggrepel)
library(ggimage)
library(gridExtra)
library(grid)
library(RCurl)
library(png)
library(jpeg)
library(ggridges)
library(ggtext)

Sys.setenv(CFBD_API_KEY = Sys.getenv("CFBD_API_KEY"))

pbp <- data.frame()

for(i in 2024) {
  re <- cfbfastR::load_cfb_pbp(seasons = i) 
  pbp <- bind_rows(pbp, re)
}

rm(re)

############## POWER 5 ###########################################

pwr <- pbp %>%
  dplyr::filter(offense_conference %in% c("SEC", "ACC", "Big 12", "Big Ten"))

top_qb <- pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name, pos_team) %>%
  summarize(attempts = n()) %>%
  arrange(desc(attempts)) %>%
  ungroup() %>%
  group_by(pos_team) %>%
  top_n(n = 1)

qb_epa <- pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name, pos_team) %>%
  summarize(total_EPA = median(EPA, na.rm = TRUE),
            var_EPA = var(EPA, na.rm = TRUE))

top_qb <- top_qb %>%
  dplyr::inner_join(qb_epa, by = "passer_player_name") %>%
  dplyr::arrange(desc(total_EPA))

df <- cfbd_team_info()

link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='20'/>")
}

p5_avg <- pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  dplyr::summarize(EPA = median(EPA, na.rm = TRUE)) %>%
  as_tibble()

# Use a unique variable for Power 5 data
p5_plot_df <- pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  left_join(top_qb, by = "passer_player_name") %>%
  left_join(df, by = c("pos_team.x" = "school")) %>%
  dplyr::filter(attempts > 0) %>%
  mutate(label = link_to_img(logo))

# Store the Power 5 plot in an object
p5_plot <- p5_plot_df %>%
  ggplot() + 
  aes(x = EPA, y = fct_reorder(label, total_EPA, .desc = FALSE), fill = color,
      color = alt_color, alpha = 0.5) + 
  geom_density_ridges(
    quantile_lines = TRUE,
    quantile_fun = function(x, ...) median(x),
    scale = 1, linewidth = 0.25
  ) +
  geom_vline(data = function(x) pwr %>%
               dplyr::filter(complete.cases(passer_player_name)) %>%
               dplyr::summarize(EPA_median = median(EPA, na.rm = TRUE)),
             aes(xintercept = EPA_median), color = "grey", linetype = "dashed", linewidth = 0.3) +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(offense_conference ~ ., scales = "free_y", ncol = 5) +
  scale_x_continuous(limits = c(-5, 5)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(margin = margin(t = -20, unit = "pt")),
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5)
  ) +
  labs(
    y = NULL,
    title = "Power 5 Passing EPA Distribution (Thru Week 7)",
    caption = "Viz: @QuinnsWisdom, updated by Ray Carpenter | Data: cfbfastR",
    subtitle = "Ordered by Starting QB w/ Top Mean Passing EPA per Conference",
    tag = "Grey Dashed Line: P5 Pass EPA Median"
  )

# Print the Power 5 plot
print(p5_plot)

############## GROUP OF 5 #############################

g5 <- pbp %>%
  dplyr::filter(offense_conference %in% c("American Athletic", "Conference USA", "Mid-American",
                                          "Mountain West", "Sun Belt"))

top_qb_g5 <- g5 %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name, pos_team) %>%
  summarize(attempts = n()) %>%
  arrange(desc(attempts)) %>%
  ungroup() %>%
  group_by(pos_team) %>%
  top_n(n = 1)

qb_epa_g5 <- g5 %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name, pos_team) %>%
  summarize(total_EPA = mean(EPA, na.rm = TRUE),
            var_EPA = var(EPA, na.rm = TRUE),
            n = n())

top_qb_g5 <- top_qb_g5 %>%
  dplyr::inner_join(qb_epa_g5, by = c("passer_player_name" = "passer_player_name", "attempts" = "n")) %>%
  dplyr::arrange(desc(total_EPA)) %>%
  dplyr::select(-pos_team.y)

g5_plot_df <- g5 %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  inner_join(top_qb_g5, by = c("passer_player_name" = "passer_player_name", "pos_team" = "pos_team.x")) %>%
  inner_join(df, by = c("pos_team" = "school")) %>%
  dplyr::filter(attempts > 0) %>%
  mutate(label = link_to_img(logo))
