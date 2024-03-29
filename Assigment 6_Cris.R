library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(bcmaps)
library(bcdata)

ldd_dubois <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()

ldb_fence_dubois2 <- bcdc_query_geodata("946b09a4-c63e-48eb-b322-3701c8ba161d", crs = 3005) %>% 
  filter(INTERSECTS(ldd_dubois)) %>% 
  collect()

mapview(ldb_fence_dubois2)

#########
## 4/4 ##
#########

#•	Calculate the length (in km) of all of the fence lines within the Lac Du Bois Grasslands Protected Area

fence_geom <- st_geometry(ldb_fence_dubois2)
dubois_geom <- st_geometry(ldd_dubois)

distance_clip <- st_intersection(dubois_geom, fence_geom)
sum(st_length(distance_clip))/1000

#########
## 2/2 ##
#########

#the length of all of the fence lines within the Lac Du Bois Grasslands Protected Area is 113.3084 Km

#•	Generate a histogram of the individual lengths of the fence lines. Include a comment detailing what you notice about this histogram.

fence_indiv_lengths <- ldb_fence_dubois2 %>%
  st_length() %>%
  as.numeric()

ggplot(data.frame(length = fence_indiv_lengths), aes(x = length)) +
  geom_histogram(binwidth = 200, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Fence Line Lengths",
       x = "Length",
       y = "Frequency")

#The histogram shows that most of the fence lines have a length less than 1000 m. The frequency of fence lines with lengths longer than 2000 m is less than 5.
#We can conclude most of the fence lines have a maximum length of 200 m

#########
## 2/2 ##
#########

#•	Create a mapview of the fence lines. Color fence lines that are less than 200m using one color, and fence lines greater than 200m another color.

library(dplyr)

ldb_fence_dubois2 <- ldb_fence_dubois2 %>%
  mutate(FEATURE_LENGTH_M = st_length(geometry),
         color_group = ifelse(as.numeric(FEATURE_LENGTH_M) < 200, "Short", "Long"))
mapview(ldb_fence_dubois2, zcol = "color_group", col.regions = c("yellow", "purple"))

#########
## 5/5 ##
#########


## TOTAL:

###########
## 13/13 ##
###########
