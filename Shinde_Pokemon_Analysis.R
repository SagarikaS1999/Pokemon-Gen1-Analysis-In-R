# Author: Sagarika Shinde

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(packcircles)

setwd("C:/Users/sagar/Downloads")

pokemon_data <- read_csv("pokemon_data.csv")

#All Pokémons of Generation 1
gen1_pokemon <- pokemon_data %>% filter(generation == 1)

#Function to generate a color palette
generate_palette <- function(n) {
  colors <- c("#f4b400", "#FF7F00", "#458B74", "#7AC5CD") #took colors similar to all the pokemons
  if (n > length(colors)) {
    colors <- colorRampPalette(colors)(n)
  }
  return(colors)
}

#Bar Plot
#1. Type Distribution of Generation 1 Pokémon
type_counts <- gen1_pokemon %>%
  select(type1, type2) %>%
  pivot_longer(cols = c(type1, type2), names_to = "type_category", values_to = "type") %>%
  filter(!is.na(type)) %>%
  count(type, name = "count") %>%
  arrange(desc(count))

ggplot(type_counts, aes(x = reorder(type, -count), y = count, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = generate_palette(length(unique(type_counts$type)))) +
  labs(
    title = "A Rainbow of Power: Exploring the Type Arsenal of Gen 1 Pokémon",
    x = "Type", y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(), # Remove grid lines
    panel.background = element_rect(fill = "white", color = NA), # Set white background
    plot.background = element_rect(fill = "white", color = NA),  # Set white background outside plot area
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#13294b"),
    axis.title = element_text(size = 14, color = "#13294b"),
    plot.title = element_text(size = 16, color = "#13294b", face = "bold")
  )

#Box Plot
#2. The Battle of Types: Offensive Strength
ggplot(gen1_pokemon, aes(x = reorder(type1, -attack, median), y = attack, fill = type1)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  scale_fill_manual(values = generate_palette(length(unique(gen1_pokemon$type1)))) +
  labs(
    title = "Clash of Titans: Which Pokémon Types Pack the Biggest Punch?",
    x = "Primary Type", y = "Attack Stats"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#13294b"),
    axis.title = element_text(size = 14, color = "#13294b"),
    plot.title = element_text(size = 16, color = "#13294b", face = "bold")
  )

#Donut Charts to see evolution line in pokemons
#Function to generate donut charts
generate_donut_chart <- function(data, title) {
  ggplot(data, aes(x = 2, y = Value, fill = Stat)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(x = 2.2, label = paste0(Stat, ": ", Value)),
      position = position_stack(vjust = 0.5), size = 2.5, color = "#13294b"
    ) +
    facet_wrap(~name) +
    xlim(1, 2.5) +
    scale_fill_manual(values = generate_palette(length(unique(data$Stat)))) +
    theme_void() +
    labs(title = title, fill = "Stat") +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#13294b"),
      legend.position = "bottom"
    )
}

#3. Donut Chart for Bulbasaur Line
bulbasaur_line <- pokemon_data %>%
  filter(name %in% c("Bulbasaur", "Ivysaur", "Venusaur"))

base_stats <- bulbasaur_line %>%
  select(name, hp, attack, defense, sp_atk, sp_def, speed) %>%
  pivot_longer(cols = -name, names_to = "Stat", values_to = "Value")

generate_donut_chart(base_stats, "From Seedling to Beast: The Evolutionary Powerhouse of Bulbasaur")

#4. Donut Chart for Pidgey Line
pidgey_line <- pokemon_data %>%
  filter(name %in% c("Pidgey", "Pidgeotto", "Pidgeot"))

#Setting factor levels to ensure correct order
pidgey_line$name <- factor(pidgey_line$name, levels = c("Pidgey", "Pidgeotto", "Pidgeot"))

pidgey_base_stats <- pidgey_line %>%
  select(name, hp, attack, defense, sp_atk, sp_def, speed) %>%
  pivot_longter(cols = -name, names_to = "Stat", values_to = "Value")

generate_donut_chart(pidgey_base_stats, "Wings of Fury: The Rise of Pidgey to Pidgeot")

#5. Donut Chart for Pikachu Line
pikachu_line <- pokemon_data %>%
  filter(name %in% c("Pikachu", "Raichu"))

pikachu_base_stats <- pikachu_line %>%
  select(name, hp, attack, defense, sp_atk, sp_def, speed) %>%
  pivot_longer(cols = -name, names_to = "Stat", values_to = "Value")

generate_donut_chart(pikachu_base_stats, "From Sparks to Thunderbolts: Pikachu’s Electrifying Evolution")

#6. Donut Chart for Charmander Line
charmander_line <- pokemon_data %>%
  filter(name %in% c("Charmander", "Charmeleon", "Charizard"))

#Setting factor levels to ensure correct order
charmander_line$name <- factor(charmander_line$name, levels = c("Charmander", "Charmeleon", "Charizard"))

charmander_base_stats <- charmander_line %>%
  select(name, hp, attack, defense, sp_atk, sp_def, speed) %>%
  pivot_longer(cols = -name, names_to = "Stat", values_to = "Value")

generate_donut_chart(charmander_base_stats, "Fanning the Flames: The Meteoric Rise of Charmander to Charizard")

#Bar Plot
#7. Catch Rate Comparison
comparison_data <- pokemon_data %>%
  filter(name %in% c(
    "Bulbasaur", "Ivysaur", "Venusaur",
    "Charmander", "Charmeleon", "Charizard",
    "Pidgey", "Pidgeotto", "Pidgeot",
    "Pikachu", "Raichu"
  ))

ggplot(comparison_data, aes(x = name, y = catch_rate, fill = name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = generate_palette(length(unique(comparison_data$name)))) +
  labs(
    title = "Catch Me If You Can: The Hunt for Iconic Pokémon",
    x = "Pokémon", y = "Catch Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#13294b"),
    axis.title = element_text(size = 14, color = "#13294b"),
    plot.title = element_text(size = 16, color = "#13294b", face = "bold")
  )

#Circular Packing Plot
#8. To see all the Pokémon abilities
# Filter Pokémon dataset for selected Pokémons
selected_pokemon <- c(
  "Bulbasaur", "Ivysaur", "Venusaur",
  "Charmander", "Charmeleon", "Charizard",
  "Pidgey", "Pidgeotto", "Pidgeot",
  "Pikachu", "Raichu"
)

pokemon_filtered <- pokemon_data %>%
  filter(name %in% selected_pokemon) %>%
  select(name, ability1, ability2, hidden_ability, total) %>%
  pivot_longer(cols = starts_with("ability"), names_to = "Ability_Type", values_to = "Ability") %>%
  filter(!is.na(Ability))

#Summarizing data for circular packing
circle_data <- pokemon_filtered %>%
  group_by(Ability) %>%
  summarise(
    Total = sum(total), 
    Count = n(), 
    Names = paste(unique(name), collapse = ", "), 
    .groups = "drop"
  )

#Generating the packing layout
packing <- circleProgressiveLayout(circle_data$Total, sizetype = "area")
circle_data <- cbind(circle_data, packing)

#Generating the vertices for the circular packing
circle_vertices <- circleLayoutVertices(packing, npoints = 50)

ggplot() +
  geom_polygon(data = circle_vertices, aes(x, y, group = id, fill = as.factor(id)), color = "white", alpha = 0.7) +
  geom_text(data = circle_data, aes(x, y, label = paste(Ability, "\n", Names)), size = 3, color = "#13294b") +
  scale_fill_manual(values = generate_palette(length(unique(circle_data$Ability)))) +
  theme_void() +
  labs(
    title = "Hidden Powers: The Ability Advantage in Pokémon Battles",
    fill = "Ability"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#13294b"),
    legend.position = "none"
  )