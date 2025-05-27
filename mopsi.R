library(tidyverse)
library(here)
library(sf)
library(reticulate)
library(leaflet)
library(osmdata)
hdb <- import("hdbscan")
pacmap <- import("pacmap")


filter_bbox <- function(data, bbox, coord_cols = c("x", "y")) {
  if(is.null(names(bbox))) {
    names(bbox)  <- c("xmin", "ymin", "xmax", "ymax")
  }
  
  data[which(data[[coord_cols[1]]] >= bbox["xmin"] &
               data[[coord_cols[2]]] >= bbox["ymin"] &
               data[[coord_cols[1]]] <= bbox["xmax"] &
               data[[coord_cols[2]]] <= bbox["ymax"]),]
}


url.data <- "https://cs.uef.fi/sipu/datasets/MopsiLocations2012-Joensuu.txt"


bbox <- osmdata::getbb("Joensuu, Finland")
df <- read_table(
  file    = url.data,
  col_names = c("y", "x")    
)

df.joensuu <- filter_bbox(df, bbox)


sf <- df %>%
  mutate(
    wkt = sprintf(
      "POINT(%f %f)",
      x, y
    )
  ) %>%
  st_as_sf(
    wkt    = "wkt",
    crs    = 4326,
    remove = FALSE
  )

ggplot(data = sf) +
  geom_sf(color = "steelblue",    # feste Punktfarbe
          alpha = 0.1,            # Transparenz
          size = 1) +             # Punktgröße
  theme_bw()

# leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(data = df,
#                    lng = ~y,    # in deinem sf ist y = Longitude
#                    lat = ~x,    #             x = Latitude
#                    radius = 1,
#                    color = "steelblue",
#                    fillOpacity = 0.7,
#                    clusterOptions = markerClusterOptions())

