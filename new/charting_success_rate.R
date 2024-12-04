# Load necessary libraries
library(cfbfastR)
library(cfbplotR)
library(ggplot2)
library(dplyr)
options(vsc.plot = TRUE)

# Define Power 4 conferences and target teams
power_4_conferences <- c("SEC", "ACC", "Big 12", "Big Ten")
highlight_teams <- c("Ohio State", "Penn State")

# Pull team-level stats for the 2024 season and filter by Power 4 conferences
team_stats <- cfbd_stats_season_team(year = 2024) %>%
  filter(conference %in% power_4_conferences)

# Pull play-by-play data for 2024
pbp_data <- cfbd_pbp_data(year = 2024, epa_wpa = TRUE)

# Calculate offensive and defensive success rates for Power 4 teams
success_stats <- pbp_data %>%
  filter(offense_play %in% team_stats$team | defense_play %in% team_stats$team) %>%
  # Aggregate success rates for each team separately on offense and defense
  group_by(offense_play) %>%
  summarize(
    off_success_rate = mean(success, na.rm = TRUE),
    games_played_off = n_distinct(game_id)
  ) %>%
  full_join(
    pbp_data %>%
      group_by(defense_play) %>%
      summarize(
        def_success_rate = mean(success, na.rm = TRUE),
        games_played_def = n_distinct(game_id)
      ),
    by = c("offense_play" = "defense_play")
  ) %>%
  rename(team = offense_play) %>%
  filter(team %in% team_stats$team) %>%  # Ensure only Power 4 teams are included
  mutate(highlight = if_else(team %in% highlight_teams, TRUE, FALSE))  # Flag for highlighted teams

# Plot Offensive Success Rate vs Defensive Success Rate
success_plot <- ggplot(success_stats, aes(x = off_success_rate, y = def_success_rate)) +
  geom_cfb_logos(aes(team = team, alpha = highlight), width = 0.05) +  # Set alpha based on highlight
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.2), guide = "none") +  # Full opacity for highlighted teams, reduced for others
  labs(
    title = "Offensive vs Defensive Success Rate (2024 Season P4 Through Week 8)",
    subtitle = "Graph by Ray Carpenter | 14thstreetanalytics.substack.com | Data from cfbfastR",
    x = "Offensive Success Rate",
    y = "Defensive Success Rate"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.05)))

# Set plot dimensions and print
options(repr.plot.width = 16, repr.plot.height = 4)
print(success_plot)

# Save the plot
ggsave("offensive_vs_defensive_success_rate_2024_highlighted_power_4.png", plot = success_plot, width = 16, height = 4)