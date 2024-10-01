
## AAS summative code: map charts ==============================================

## preliminaries ---------------------------------------------------------------

library(tidyverse)
library(maps)
library(mapdata)
library(countrycode)
library(igraph)

## code adapted from: https://datascience.blog.wzb.eu/2018/05/31/three-ways-of-visualizing-a-graph-on-a-map/ 

## map theme -------------------------------------------------------------------

maptheme <- theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "bottom",
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = unit(c(0, 0, 0.5, 0), 'cm')
)

## map function ----------------------------------------------------------------

create_migration_map <- function(data, orig_filter, year_min_filter, event) {
  
  # Filter data
  mapdat <- data %>%
    dplyr::select(orig, dest, year, forced_mig) %>%
    filter(orig == orig_filter) %>%
    filter(year >= year_min_filter) %>%
    drop_na(forced_mig)
  
  # Get unique countries
  countries <- union(mapdat$orig, mapdat$dest)
  
  # Nodes dataframe with geog information
  nodes <- data.frame(id = 1:length(countries), name = countries)
  
  # Get world map
  wmap <- getMap(resolution = "high")
  
  # Get centroids
  centroids <- gCentroid(wmap, byid = TRUE)
  
  # Data frame with centroids
  centroids_df <- as.data.frame(centroids) %>%
    rownames_to_column(var = "name") %>%
    mutate(name = countrycode(name, "country.name", "iso3c")) %>%
    rename(lon = x, lat = y)
  
  # Merge nodes with centroids
  nodes <- nodes %>% left_join(centroids_df, by = "name")
  
  # Creating edges
  edges <- mapdat %>%
    group_by(orig, dest) %>%
    summarise(forced_mig = sum(forced_mig, na.rm = TRUE))
  
  all_vertices <- union(edges$orig, edges$dest)
  nodes <- filter(nodes, name %in% all_vertices)
  
  # Creating graph
  graph <- graph_from_data_frame(edges, directed = TRUE, vertices = all_vertices)
  
  # Adding id info
  edges_for_plot <- edges %>%
    inner_join(nodes %>% select(name, lon, lat), by = c('orig' = 'name')) %>%
    rename(x = lon, y = lat) %>%
    inner_join(nodes %>% select(name, lon, lat), by = c('dest' = 'name')) %>%
    rename(xend = lon, yend = lat)
  
  # World map
  country_shapes <- geom_polygon(
    aes(x = long, y = lat, group = group),
    data = map_data('world'),
    fill = "#CECECE", color = "#515151",
    size = 0.15
  )
  mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
  
  # Create the map
  migration_map <- ggplot(nodes) + country_shapes +
    geom_curve(
      aes(x = x, y = y, xend = xend, yend = yend),
      data = edges_for_plot, curvature = 0.33,
      alpha = 0.5
    ) +
    scale_size_continuous(guide = FALSE, range = c(0.25, 2)) +
    geom_point(
      aes(x = lon, y = lat),
      shape = 21, fill = 'white',
      color = 'black', stroke = 0.5
    ) +
    scale_size_continuous(guide = FALSE, range = c(1, 6)) +
    #geom_text(
      #aes(x = lon, y = lat, label = name),
      #hjust = 0, nudge_x = 1, nudge_y = 4,
      #size = 3, color = "black", fontface = "bold"
    #) +
    mapcoords + maptheme +
    labs(
      title = paste("Forced Migrant Flows:", event)
    ) +
    theme_few()
  
  return(migration_map)
}

MMR_map = create_migration_map (dat_unimputed, "MMR", "1978", "Rohingya Refugee Crisis")
SYR_map = create_migration_map (dat_unimputed, "SYR", "2011", "Syrian Civil War")
SOM_map = create_migration_map (dat_unimputed, "SOM", "1991", "Somali Civil War")


## displaying them all together
grid.arrange (MMR_map, SYR_map, SOM_map, nrow = 3)

