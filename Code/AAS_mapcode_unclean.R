
## AAS summative code: map charts ==============================================

## preliminaries ---------------------------------------------------------------

## clearing global environment
rm (list = ls())

## installing packages
pacman::p_load (tidyverse, countrycode, naniar, ggthemes, corrplot, robustbase, 
                gridExtra, leaflet, leaflet.extras, rnaturalearth, rnaturalearthdata,
                htmlwidgets, maps, geonames, rworldmap, rgeos, igraph)

library (leaflet)
library (leaflet.extras)
library (rnaturalearth)

## map visuals -----------------------------------------------------------------

## data
mapdat = dat_unimputed %>%
  
  # isolating relevant variables
  dplyr::select (orig, dest, year, forced_mig) %>%
  
  # isolating Myanmar
  filter (orig == "SOM") %>%
  filter (year >= 1991)
  
  # remove NAs
  drop_na (forced_mig) 

countries = union (mapdat$orig, mapdat$dest)

## nodes df with geog information
nodes = data.frame (id = 1:length(countries), 
                    name = countries)

## get world map 
wmap <- getMap (resolution = "high")

## get centroids 
centroids <- gCentroid (wmap, byid = TRUE)

## df with centroids
centroids_df = as.data.frame (centroids) %>%
  
  # rownames to column
  rownames_to_column (var = "name") %>%
  
  # converting to ISO3C
  mutate (name = countrycode (name, "country.name", "iso3c")) %>%
  
  # renaming
  rename (lon = x,
          lat = y)

nodes = nodes %>% left_join (centroids_df, by = "name")

## creating edges
edges = mapdat %>%
  dplyr::group_by (orig, dest) %>%
  dplyr::summarise (forced_mig = sum (forced_mig, na.rm = T))

all_vertices <- union(edges$orig, edges$dest)
nodes <- filter (nodes, name %in% all_vertices)

## creating graph
graph <- graph_from_data_frame (edges, directed = TRUE, vertices = all_vertices)

## adding id info
edges_for_plot <- edges %>%
  inner_join (nodes %>% select (name, lon, lat), by = c('orig' = 'name')) %>%
  rename (x = lon, y = lat) %>%
  inner_join (nodes %>% select (name, lon, lat), by = c('dest' = 'name')) %>%
  rename (xend = lon, yend = lat)

## theme or displaying map
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

## world map
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

som_map = ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "black", fontface = "bold") +
  mapcoords + maptheme +
  labs (
    title = "Forced Migrant Flows: Somali Civil War"
  ) + 
  theme_few ()

som_map

grid.arrange (mmr_map, syr_map, som_map, nrow = 3)







