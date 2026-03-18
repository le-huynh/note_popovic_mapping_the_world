#'---
#' title: Chap05 - Points map
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
    rio,            # import and export files
    here,           # locate files 
    tidyverse,      # data management and visualization
    rnaturalearth,
    sf,
    maps,            # for simple city data
    ggrepel         # labeling
)

#' # Basic map
# basic map #-------------
#' ## Data
## data #-----------
#' Get world boundaries for background map
(world_map_background <- rnaturalearth::ne_countries(scale = 'medium',
                                                    returnclass = 'sf') %>% 
    dplyr::select(iso_a2, name, geometry))

#' Get world city data: `maps::world.cities` is smaller than `rnaturalearth` cities
maps::world.cities %>% tibble()

(cities_df <- maps::world.cities %>% 
    dplyr::select(name, country.etc, pop, lat, long) %>% 
    st_as_sf(coords = c("long", "lat"),
             crs = 4326))

#' ## Simple dot map
## simple dot map #-----------------------
fig1 <- ggplot() +
    # world map background
    geom_sf(data = world_map_background,
            aes(geometry = geometry),
            fill = "grey80",
            color = "white",
            linewidth = 0.1) +
    # city points
    geom_sf(data = cities_df,
            aes(geometry = geometry),
            color = "tomato",
            size = 1,
            shape = 16,
            alpha = 0.6) +
    labs(title = "World cities") +
    theme_minimal() +
    theme(axis.text = element_blank())

fig1

#' ## Bubble map
## bubble map #-----------------------
fig2 <- ggplot() +
    # world map background
    geom_sf(data = world_map_background,
            aes(geometry = geometry),
            fill = "grey80",
            color = "white",
            linewidth = 0.1) +
    # city points - map population to size
    geom_sf(data = cities_df %>% filter(pop > 1000000),
            aes(geometry = geometry,
                size = pop),
            color = "dodgerblue",
            shape = 16,
            alpha = 0.6) +
    # control the size scaling and legend
    scale_size_area(name = "Population",
                    # max bubble size on plot
                    max_size = 5,
                    # format labels (millions)
                    labels = scales::label_number(scale = 1e-6,
                                                  suffix = "M")) +
    labs(title = "World cities sized by population (> 1 million)",
         caption = "Data: maps::world.citites") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          legend.position = "bottom")

fig2

#' ## Handling labels with `ggrepel`
## handle labels #----------------------
fig3 <- fig2 +
    # add labels
    ggrepel::geom_text_repel(data = cities_df %>% filter(pop >= 10000000),
                             aes(label = name,
                                 geometry = geometry),
                             # get coordinates
                             stat = "sf_coordinates",
                             size = 2.5,
                             # draw line even if label is close to point
                             min.segment.length = 0,
                             # allow some overlap if needed, increase if too sparse
                             max.overlaps = 30,
                             # how strongly labels push away
                             force = 0.5,
                             # padding around text
                             box.padding = 0.2
                             ) +
    labs(subtitle = "Labelled cities >= 10M",
         x = NULL,
         y = NULL)

fig3

