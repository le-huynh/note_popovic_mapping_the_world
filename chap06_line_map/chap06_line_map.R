#'---
#' title: Chap06 - Line and network map
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
    sf,
    osmdata,
    ggspatial,
    patchwork
)

#' # Motorways in Shenzhen, China
# motorways #-------------
#' ## Data
#' Get bounding box for Shenzhen, China
city_bbox <- osmdata::getbb("Shenzhen, China")
city_bbox

#' Build and run OSM query for motorways within the city
# start the query for the bounding box
osm_motorways_query1 <- osmdata::opq(bbox = city_bbox,
                                     timeout = 60)

osm_motorways_query1

# specify tag highway=motorway
osm_motorways_query2 <- osm_motorways_query1 %>% 
    osmdata::add_osm_feature(key = "highway",
                             value = "motorway")

osm_motorways_query2

# execute the query
osm_motorways_query <- osm_motorways_query2 %>% 
    osmdata::osmdata_sf()

osm_motorways_query

#' ## Extract the lines object
city_motorways_sf <- osm_motorways_query$osm_lines

city_motorways_sf %>% tibble()

#' ## Basic plot
plot(sf::st_geometry(city_motorways_sf))

#' ## Styling lines
dev.off()
fig1 <- ggplot() +
    geom_sf(data = city_motorways_sf,
            aes(geometry = geometry),
            color = "darkgoldenrod",
            linewidth = 0.6) +
    labs(title = "Motorways in Shenzhen",
         caption = "Data: OpenStreetMap contributors") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank())

fig1

city_motorways_sf %>% count(surface)

(city_motorways_sf1 <- city_motorways_sf %>% 
    mutate(surface = replace_na(surface, "NA")) %>% 
    tibble())

fig2 <- ggplot() +
    geom_sf(data = city_motorways_sf1,
            aes(geometry = geometry,
                color = surface,
                linewidth = surface),
            alpha = 0.8) +
    scale_color_manual(values = c("asphalt"= "#36842EFF",
                                  "concrete" = "#A60007FF",
                                  "paved" = "#23719CFF",
                                  "NA" = "#68317EFF")) +
    scale_linewidth_manual(values = c("asphalt"= 1,
                                      "concrete" = 0.5,
                                      "paved" = 1.5,
                                      "NA" = 0.1)) +
    labs(title = "Motorways in Shenzhen",
         caption = "Data: OpenStreetMap contributors") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank())

fig2

#' ## Line simplification
#' Check current CRS
original_crs <- sf::st_crs(city_motorways_sf)
original_crs

#' Simplify the lines
tolerance_meters <- 5

city_motorways_sf_simplified <- sf::st_simplify(city_motorways_sf,
                                                dTolerance = tolerance_meters)
#' Plot original lines
fig_original <- ggplot() +
    geom_sf(data = city_motorways_sf,
            aes(geometry = geometry),
            linewidth = 0.5) +
    labs(title = "Original Motorways") +
    theme_void()

#' Plot original lines
fig_simplified <- ggplot() +
    geom_sf(data = city_motorways_sf_simplified,
            aes(geometry = geometry),
            linewidth = 0.5,
            color = "cyan4") +
    labs(title = "Simplified Motorways (5m tolerance)") +
    theme_void()

#' Combined plot
fig_original + fig_simplified

#' # Simple flow map
# flow map #----------------
#' Define start and end points
origin_x <- 4.86 # Approx. Longitude near Vondelpark West 
origin_y <- 52.358 # Approx. Latitude 
dest_x <- 4.88 # Approx. Longitude near Vondelpark East 
dest_y <- 52.36

#' Data frame for flow segment
(flow_data <- data.frame(x_start = origin_x,
                        y_start = origin_y,
                        x_end = dest_x,
                        y_end = dest_y,
                        volume = 100 # fictional flow volume
                        ))

#' Plot flow map
ggplot(data = flow_data) +
    geom_segment(aes(x = x_start,
                     y = y_start,
                     xend = x_end,
                     yend = y_end,
                     # map volume to linewidth (thickness)
                     linewidth = volume),
                 color = "cyan4",
                 alpha = 0.8,
                 # add arrowhead
                 arrow = arrow(length = unit(0.3, "cm"),type = "closed")) +
    # control how volume maps to thickness 
    scale_linewidth(# min/max thickness
                    range = c(0.5, 3),
                    name = "Flow volume" ) +
    # add points for origin/destination
    geom_point(aes(x = x_start, y = y_start),
               color = "darkgreen",
               size = 3) + 
    geom_point(aes(x = x_end, y = y_end), 
               color = "darkred",
               size = 3) +
    labs(title = "Simple Flow Map Example (Conceptual)") +
    # use coord_map() or coord_sf() if plotting on a real map background
    coord_fixed(ratio = 1.6) +
    theme_minimal()

#' # River map
# river map #----------------
#' Get line data (OSM river)
(bbox <- osmdata::getbb("Kolkata, India"))

osm_rivers_query <- osmdata::opq(bbox = bbox,
                                 timeout = 120) %>% 
    # Increase timeout 
    osmdata::add_osm_feature(key = "waterway",
                             value = "river") %>% 
    osmdata::osmdata_sf() 

kolkata_rivers_sf <- osm_rivers_query$osm_lines

#' Data cleaning
kolkata_rivers_sf_clean <- kolkata_rivers_sf %>%
    dplyr::select(osm_id, name, waterway, geometry) %>%
    dplyr::filter(!sf::st_is_empty(geometry))

#' Plot river
fig_river <- ggplot() +
    geom_sf(data = kolkata_rivers_sf_clean,
            aes(geometry = geometry),
            color = "steelblue",
            linewidth = 0.4) +
    labs(title = "Major Rivers in Kolkata, India",
         caption = "Data: OpenStreetMap contributors") +
    # add scale bar
    ggspatial::annotation_scale(location = "bl",
                                width_hint = 0.4,
                                style = "ticks") +
    theme_void()

fig_river

