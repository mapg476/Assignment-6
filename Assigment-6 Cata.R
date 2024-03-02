#Reading libraries

library("tidyverse")
library("sf")
library("terra")
library("mapview")
library("bcdata")
library("bcmaps")
library("ggplot2")

#Getting the Lac Du Bois Grasslands Protected Area layer
ldb <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
collect()

#Getting the BEC layer
bec <- bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3", crs = 3005) |> 
  filter(INTERSECTS(ldb)) |> 
  collect()

##Calculate the total area of each of the resulting features in hectares

#Clip between bec and ldb
clip_bec_ldb <- st_intersection(bec,ldb)

#sum of areas
total_area <- sum(st_area(clip_bec_ldb))/10000


# Create a bar plot where the “MAP_LABEL” column is along the X-axis, and 
# the area is along the Y-axis. Display each bar using different colors.

sum_area <- clip_bec_ldb %>% 
  group_by(MAP_LABEL) %>% 
  summarise(total_area=sum(FEATURE_AREA_SQM))

ggplot(data = sum_area, aes(x = MAP_LABEL, y = total_area, fill = MAP_LABEL)) +
  geom_bar(stat = "identity") +
  labs(x = "MAP Label", y = "Area", title = "Total Area by MAP Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "MAP Label")

# Extract the mean elevation of each of the features (you will need to pull in 
# the DEM from the “cded_terra” function)
ldb_dem <- cded_terra(ldb)
ldb_dem
ldb_dem_albers <- project(ldb_dem,"epsg:3005")

plot(ldb_dem_albers)
terrain_feats <- terrain(ldb_dem_albers, v = c("slope", "aspect", "TPI"))
plot(terrain_feats)

ldb_dem_terrain <- c(ldb_dem_albers,terrain_feats)
ldb_dem_terrain

bec_elevation <- terra::extract(
  ldb_dem_terrain, sum_area, fun = mean, na.rm = TRUE, bind = TRUE) %>% 
  st_as_sf()

# Create a mapview of the BEC vector layer, coloring the polygons by their
# subzone label.

mapview(clip_bec_ldb,zcol="SUBZONE")
