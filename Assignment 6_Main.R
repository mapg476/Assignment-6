### Part I

## a.	Download and analyze all of the historical fire incident location points 
# that fall within the Lac Du Bois Grasslands Protected Area (you will be looking 
# for the layer named “fire incident locations – historical”)

# Read libraries

library(sf)
library(terra)
library(bcmaps)
library(bcdata)
library(tidyverse)
library(readxl)
library(mapview)
library(dplyr)
library(ggplot2)

ldb <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()

mapview(ldb)

fire <- bcdc_query_geodata("e2dadc60-292f-4d98-b42b-56ca9e4fe694", crs = 3005) |> 
  filter(INTERSECTS(ldb)) |> 
  collect()

mapview(fire)

## Summarise this dataset to show how many fires occurred by fire year

summary_fires <- fire %>%
  group_by(FIRE_YEAR) %>%
  summarise(fires_count = n())

## Create another data summary that shows the overall breakdown of fire cause
# in the Lac Du Bois Grasslands Protected Area.

fire_cause_summary <- fire %>%
  group_by(FIRE_CAUSE) %>%
  summarise(fires_count = n())

## Generate a mapview of the fire points with colors to illustrate the fire cause

mapview(fire, zcol = "FIRE_CAUSE", col.regions = rainbow(length(unique(fire$FIRE_CAUSE))))

## Create a boxplot of the mean fires per year by fire cause, i.e.: the X-axis 
# should show the different fire causes, and the Y-axis should show the mean 
# number of fires per year. Display each box using different colors

mean_fires_per_cause <- fire %>%
  group_by(FIRE_CAUSE, FIRE_YEAR) %>%
  summarise(mean_fires = n()) %>%
  group_by(FIRE_CAUSE) %>%
  summarise(mean_fires_per_year = mean(mean_fires))

ggplot(mean_fires_per_cause, aes(x = FIRE_CAUSE, y = mean_fires_per_year, fill = FIRE_CAUSE)) +
  geom_boxplot(width = 2) +
  labs(x = "Fire Cause", y = "Mean Fires per Year") +
  theme_minimal()
