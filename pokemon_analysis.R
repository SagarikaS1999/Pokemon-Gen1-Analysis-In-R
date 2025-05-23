
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2) # For data melting
library(fmsb)
library(tidyr)

setwd("C:/Users/sagar/Downloads")
# Load the Pokémon dataset
pokemon <- read_csv("pokemon_analysis_data.csv")

# 1. Distribution of Pokémon by Type
type_distribution <- pokemon %>%
  count(type1, sort = TRUE)
ggplot(type_distribution, aes(x = reorder(type1, -n), y = n, fill = type1)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pokémon by Primary Type", x = "Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Strongest Pokémon by Base Stats
strongest_pokemon <- pokemon %>%
  arrange(desc(total)) %>%
  head(10)
ggplot(strongest_pokemon, aes(x = reorder(name, total), y = total, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Strongest Pokémon by Total Stats", x = "Pokémon", y = "Total Stats") +
  coord_flip() +
  theme_minimal()

# 3. Comparison of Stats for Top 5 Pokémon
# Filter data for the top 5 Pokémon
top_5_pokemon <- pokemon %>%
  filter(name %in% c("Arceus", "Rayquaza", "Ho-oh", "Mewtwo", "Eternatus")) %>%
  select(name, attack, defense, sp_atk, sp_def, speed, hp, total)

# Reshape the data to long format for plotting
top_5_long <- top_5_pokemon %>%
  pivot_longer(cols = -name, names_to = "Stat", values_to = "Value")

# Plot: Grouped bar chart for stat comparison
ggplot(top_5_long, aes(x = Stat, y = Value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparison of Stats for Top 5 Pokémon",
    x = "Stat Type",
    y = "Stat Value",
    fill = "Pokémon"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# 4. ARCEUS
# Filter data for Arceus
arceus_stats <- pokemon %>%
  filter(name == "Arceus") %>%
  select(attack, defense, sp_atk, sp_def, speed, hp)

# Add min and max rows for scaling
arceus_radar <- rbind(
  max = rep(150, ncol(arceus_stats)),  # Set max scale for visualization
  min = rep(0, ncol(arceus_stats)),    # Set min scale for visualization
  arceus_stats
)

# Plot radar chart
radarchart(
  arceus_radar,
  axistype = 1,
  pcol = "darkgoldenrod1",
  pfcol = alpha("gold", 0.5),
  plwd = 2,
  title = "Arceus Stat Distribution"
)

# Add a legend
legend(
  "topright", legend = "Arceus", col = "darkgoldenrod1",
  lty = 1, lwd = 2, bty = "n"
)

# 5. ETERNATUS
# Filter data for ETERNATUS
arceus_stats <- pokemon %>%
  filter(name == "Eternatus") %>%
  select(attack, defense, sp_atk, sp_def, speed, hp)

# Add min and max rows for scaling
arceus_radar <- rbind(
  max = rep(150, ncol(arceus_stats)),  # Set max scale for visualization
  min = rep(0, ncol(arceus_stats)),    # Set min scale for visualization
  arceus_stats
)

# Plot radar chart
radarchart(
  arceus_radar,
  axistype = 1,
  pcol = "darkgoldenrod1",
  pfcol = alpha("gold", 0.5),
  plwd = 2,
  title = "Arceus Stat Distribution"
)

# Add a legend
legend(
  "topright", legend = "Arceus", col = "darkgoldenrod1",
  lty = 1, lwd = 2, bty = "n"
)

# 6. RAYQUAZA
# Filter data for RAYQUAZA
arceus_stats <- pokemon %>%
  filter(name == "Rayquaza") %>%
  select(attack, defense, sp_atk, sp_def, speed, hp)

# Add min and max rows for scaling
arceus_radar <- rbind(
  max = rep(150, ncol(arceus_stats)),  # Set max scale for visualization
  min = rep(0, ncol(arceus_stats)),    # Set min scale for visualization
  arceus_stats
)

# Plot radar chart
radarchart(
  arceus_radar,
  axistype = 1,
  pcol = "darkgoldenrod1",
  pfcol = alpha("gold", 0.5),
  plwd = 2,
  title = "Arceus Stat Distribution"
)

# Add a legend
legend(
  "topright", legend = "Arceus", col = "darkgoldenrod1",
  lty = 1, lwd = 2, bty = "n"
)

# 7. Ho-oh
# Filter data for Ho-oh
arceus_stats <- pokemon %>%
  filter(name == "Ho-oh") %>%
  select(attack, defense, sp_atk, sp_def, speed, hp)

# Add min and max rows for scaling
arceus_radar <- rbind(
  max = rep(150, ncol(arceus_stats)),  # Set max scale for visualization
  min = rep(0, ncol(arceus_stats)),    # Set min scale for visualization
  arceus_stats
)

# Plot radar chart
radarchart(
  arceus_radar,
  axistype = 1,
  pcol = "red",
  pfcol = alpha("gold", 0.5),
  plwd = 2,
  title = "Arceus Stat Distribution"
)

# Add a legend
legend(
  "topright", legend = "Arceus", col = "red",
  lty = 1, lwd = 2, bty = "n"
)

# 8. Mewtwo
# Filter data for Mewtwo
arceus_stats <- pokemon %>%
  filter(name == "Mewtwo") %>%
  select(attack, defense, sp_atk, sp_def, speed, hp)

# Add min and max rows for scaling
arceus_radar <- rbind(
  max = rep(150, ncol(arceus_stats)),  # Set max scale for visualization
  min = rep(0, ncol(arceus_stats)),    # Set min scale for visualization
  arceus_stats
)

# Plot radar chart
radarchart(
  arceus_radar,
  axistype = 1,
  pcol = "darkgoldenrod1",
  pfcol = alpha("gold", 0.5),
  plwd = 2,
  title = "Arceus Stat Distribution"
)

# Add a legend
legend(
  "topright", legend = "Arceus", col = "darkgoldenrod1",
  lty = 1, lwd = 2, bty = "n"
)
########################
# Filter data for specified Pokémon
top_pokemon_species <- pokemon %>%
  filter(name %in% c("Arceus", "Rayquaza", "Ho-oh", "Mewtwo", "Eternatus")) %>%
  select(name, special_group, species)

# Grouped Bar Chart with Pokémon Names
ggplot(top_pokemon_species, aes(x = species, fill = special_group)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.6), width = 0.6) +
  geom_text(
    stat = "count",  # Automatically calculates y-values (counts)
    aes(label = name),
    position = position_dodge(width = 0.6),  # Adjusts position for grouped bars
    vjust = -0.5, size = 3, color = "black", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Legendary" = "gold", "Non-Legendary" = "grey")) +
  theme_minimal() +
  labs(
    title = "Grouped Bar Chart of Special Group, Species, and Pokémon Names",
    x = "Species",
    y = "Count",
    fill = "Special Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


##############################
library(ggplot2)
library(dplyr)

# Prepare data: Count combinations of type1 and type2
type_combinations <- pokemon %>%
  group_by(type1, type2) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(type2 = ifelse(is.na(type2), "None", type2))  # Replace NA with "None"

# Heatmap for Type Combinations
ggplot(type_combinations, aes(x = type1, y = type2, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
  theme_minimal() +
  labs(
    title = "Heatmap of Pokémon Type Combinations",
    x = "Primary Type (type1)",
    y = "Secondary Type (type2)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )



















# Load necessary library
library(ggplot2)
library(dplyr)

# Filter data for the top 5 Pokémon
top_5_legendary <- pokemon%>%
  filter(name %in% c("Arceus", "Rayquaza", "Ho-oh", "Mewtwo", "Dialga"))

# Plot: Highlight Legendary Pokémon
ggplot(top_5_legendary, aes(x = attack, y = defense, color = special_group, label = name)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  theme_minimal() +
  scale_color_manual(values = c("Legendary" = "gold", "Non-Legendary" = "grey")) +
  labs(
    title = "Legendary Status of Top 5 Pokémon",
    x = "Attack",
    y = "Defense",
    color = "Special Group"
  ) +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


















# 5. Generational Trends
generation_trends <- pokemon %>%
  group_by(generation) %>%
  summarise(count = n(), average_total = mean(total, na.rm = TRUE))
ggplot(generation_trends, aes(x = generation)) +
  geom_bar(aes(y = count), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = average_total * 10, group = 1), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Average Total Stats")) +
  labs(title = "Generational Trends in Pokémon Count and Stats", x = "Generation", y = "Count") +
  theme_minimal()

# 6. Legendary vs Non-Legendary Pokémon
ggplot(pokemon, aes(x = is_legendary, y = total, fill = is_legendary)) +
  geom_boxplot() +
  labs(title = "Legendary vs Non-Legendary Pokémon Stats", x = "Legendary Status", y = "Total Stats") +
  theme_minimal()




#Can you create plots for speedsters vs tanks and top 10 easier and most difficult to catch?

# R Code for Speedsters vs. Tanks and Catch Rate Analysis
# 1. Speedsters vs Tanks
# Define thresholds
speed_threshold <- quantile(pokemon$speed, 0.75, na.rm = TRUE)
defense_threshold <- quantile(pokemon$defense, 0.75, na.rm = TRUE)

# Classify Pokémon
pokemon <- pokemon %>%
  mutate(role = case_when(
    speed >= speed_threshold & defense < defense_threshold ~ "Speedster",
    defense >= defense_threshold & speed < speed_threshold ~ "Tank",
    TRUE ~ "Balanced"
  ))

# Scatter plot of Speed vs Defense
ggplot(pokemon, aes(x = defense, y = speed, color = role)) +
  geom_point(alpha = 0.7) +
  labs(title = "Speedsters vs Tanks: Speed vs Defense", x = "Defense", y = "Speed") +
  theme_minimal() +
  scale_color_manual(values = c("Speedster" = "blue", "Tank" = "red", "Balanced" = "gray"))

# Prepare data for heatmap
heatmap_data <- top_pokemon %>%
  select(name, Attack_Gain, Defense_Gain, Speed_Gain, Total_Stat_Gain) %>%
  melt(id.vars = "name")

# Heatmap
ggplot(heatmap_data, aes(x = variable, y = name, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Heatmap of Stat Gains for Top Pokémon",
    x = "Stat Type", y = "Pokémon"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate average stats per generation
avg_stats <- pokemon %>%
  group_by(generation) %>%
  summarise(Average_Total = mean(total))

# Line plot
ggplot(avg_stats, aes(x = generation, y = Average_Total)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(size = 3, color = "darkblue") +
  theme_minimal() +
  labs(
    title = "The Evolution of Power",
    x = "Generation", y = "Average Total Stats"
  )

# Average stats by generation and stat type
avg_stat_types <- pokemon %>%
  group_by(generation) %>%
  summarise(
    Attack = mean(attack), Defense = mean(defense),
    Speed = mean(speed), HP = mean(hp)
  ) %>%
  pivot_longer(-generation, names_to = "Stat_Type", values_to = "Average")

# Stacked bar chart
ggplot(avg_stat_types, aes(x = generation, y = Average, fill = Stat_Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Stat Composition Across Generations",
    x = "Generation", y = "Average Stat Value"
  ) +
  scale_fill_brewer(palette = "Set3")



# Average stats by type
avg_by_type <- pokemon %>%
  group_by(type1) %>%
  summarise(
    Attack = mean(attack), Defense = mean(defense),
    Speed = mean(speed), Total = mean(total)
  )

# Radar chart requires scaling
radar_data <- avg_by_type %>%
  mutate_all(~ . / max(.)) %>%
  column_to_rownames(var = "type1")

radarchart(radar_data, axistype = 1,
           title = "Average Stats by Pokémon Type",
           pcol = rainbow(nrow(radar_data)))

ggplot(pokemon, aes(x = special_group, y = total, fill = special_group)) +
  geom_violin(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Legendaries vs. Ordinary Pokémon",
    x = "Group", y = "Total Stats"
  ) +
  scale_fill_manual(values = c("Ordinary" = "blue", "Legendary" = "gold"))






