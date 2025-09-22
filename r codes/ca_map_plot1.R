library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)

# Define county groups
low_income <- c("Butte","Del Norte","Humboldt","Imperial","Kern","Kings",
                "Lake","Lassen","Mariposa","Mendocino","Merced","Modoc",
                "Plumas","Sierra","Siskiyou","Tehama","Trinity","Tulare")

middle_income <- c("Amador","Calaveras","Colusa","Fresno","Glenn","Inyo",
                   "Los Angeles","Madera","Mono","Monterey","Nevada",
                   "Riverside","Sacramento","San Bernardino","San Joaquin",
                   "San Luis Obispo","Shasta","Stanislaus","Sutter",
                   "Tuolumne","Yolo","Yuba")

high_income <- c("Alameda","Alpine","Contra Costa","El Dorado","Marin","Napa",
                 "Orange","Placer","San Benito","San Diego","San Francisco",
                 "San Mateo","Santa Barbara","Santa Clara","Santa Cruz",
                 "Solano","Sonoma","Ventura")

# Get CA county map data
ca_map <- map_data("county", region = "california")

# Standardize county names (map_data uses lowercase names with spaces)
ca_map$subregion <- tools::toTitleCase(ca_map$subregion)

# Build a data frame of groups
county_groups <- data.frame(
  county = unique(ca_map$subregion),
  income_group = "Missing",
  stringsAsFactors = FALSE
)

county_groups$income_group[county_groups$county %in% low_income] <- "Low"
county_groups$income_group[county_groups$county %in% middle_income] <- "Middle"
county_groups$income_group[county_groups$county %in% high_income] <- "High"

# Merge into map data
ca_map <- left_join(ca_map, county_groups, by = c("subregion" = "county"))

# Plot with custom colors
ggplot(ca_map, aes(long, lat, group = group, fill = income_group)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual(values = c(
    "Low" = "#F0784B",      # orange
    "Middle" = "lightblue", # user request
    "High" = "#286576",     # teal
    "Missing" = "black"
  )) +
  labs(fill = "Income Group")
