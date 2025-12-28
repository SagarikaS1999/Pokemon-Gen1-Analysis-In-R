# Pokémon Gen 1 Visual Analysis in 

This project explores Generation 1 Pokémon through the lens of data science and visual storytelling. By leveraging the Kaggle Pokémon dataset and R-based visualization libraries, the analysis delves into type distributions, base stats, evolutionary progressions, and legendary status comparisons — culminating in an academic poster titled **"Exploring the World of Pokémon: Insights from Gen 1"**.

---

## Project Overview

This project focuses on:
- Understanding **type distributions** among Gen 1 Pokémon
- Analyzing **base stat progression** through evolutionary lines (e.g., Bulbasaur, Charmander)
- Highlighting **battle strength** through attack-focused box plots
- Comparing **catch rates** across iconic species
- Visualizing **abilities** using circular packing plots

---
## Data Storytelling

<img width="1442" height="961" alt="image" src="https://github.com/user-attachments/assets/345eacf3-a86d-472a-8ee7-a75dc7488a6a" />

---

## Dataset

- **Source**: [Kaggle Pokémon Stats Dataset](https://www.kaggle.com/datasets/guavocado/pokemon-stats-1025-pokemons)
- **Filtered for**: Generation 1 Pokémon
- **Attributes Used**:
  - Base stats: `hp`, `attack`, `defense`, `sp_atk`, `sp_def`, `speed`, `total`
  - Typing: `type1`, `type2`
  - Evolution lines
  - Catch rate and Abilities

---

##  Key Visuals in This Project

| Visualization | Description |
|---------------|-------------|
| 📊 Bar Chart | Distribution of Pokémon types |
| 🧪 Box Plot | Attack stats grouped by type |
| 🍩 Donut Charts | Stat distribution across evolution lines (Bulbasaur, Pikachu, etc.) |
| 🔥 Catch Rate Plot | Comparing how easy/hard iconic Pokémon are to catch |
| 🧬 Circular Packing | Exploring ability overlaps and uniqueness |
  
---

## Files

- `Shinde_Pokemon_Analysis.R` – Main R script with all plots and logic
- `pokemon_data.csv` – Cleaned dataset used for Gen 1 filtering
- `poster copy.pdf` – Final academic poster output for IST 719

---

## Tools & Packages

Built using R and the following packages:
- `ggplot2` – Data Visualization
- `dplyr`, `tidyr` – Data manipulation
- `packcircles` – Circular packing for ability plots
- `readr` – Data import

---

## 🧠 Project Motivation

> “This project is for Pokémon fans and data storytellers alike. Whether you're building competitive teams or learning how to use R for compelling storytelling, this project uncovers the hidden structure and evolution of some of the most iconic Pokémon of all time.”
