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

# Calculate total passing and rushing yards allowed and games played for Power 4 teams
defensive_stats <- pbp_data %>%
  filter(defense_play %in% team_stats$team) %>%  # Filter for Power 4 teams
  group_by(defense_play) %>%
  summarize(
    total_passing_yards_allowed = sum(yards_gained[pass == 1], na.rm = TRUE),
    total_rushing_yards_allowed = sum(yards_gained[rush == 1], na.rm = TRUE),
    games_played = n_distinct(game_id)
  ) %>%
  mutate(
    passing_yards_allowed_per_game = total_passing_yards_allowed / games_played,
    rushing_yards_allowed_per_game = total_rushing_yards_allowed / games_played,
    highlight = if_else(defense_play %in% highlight_teams, TRUE, FALSE)  # Flag for highlighted teams
  )

# Plot Passing Yards Allowed vs Rushing Yards Allowed per Game
defense_plot <- ggplot(defensive_stats, aes(x = passing_yards_allowed_per_game, y = rushing_yards_allowed_per_game)) +
  geom_cfb_logos(aes(team = defense_play, alpha = highlight), width = 0.05) +  # Set alpha based on highlight
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.2), guide = "none") +  # Full opacity for highlighted teams, reduced for others
  labs(
    title = "Passing vs Rushing Yards Allowed per Game (2024 Season)",
    subtitle = "Graph by Ray Carpenter | 14thstreetanalytics.substack.com | Data from cfbfastR",
    x = "Passing Yards Allowed per Game",
    y = "Rushing Yards Allowed per Game"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

# Set plot dimensions and print
options(repr.plot.width = 16, repr.plot.height = 4)
print(defense_plot)

# Save the plot
ggsave("passing_vs_rushing_yards_allowed_2024_highlighted_power_4.png", plot = defense_plot, width = 16, height = 4)
