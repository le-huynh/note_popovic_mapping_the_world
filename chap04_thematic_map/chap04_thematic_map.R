#'---
#' title: Chap04 - Thematic map
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
    rnaturalearthdata,
    sf,
    ggspatial,      # add Scale bars/North arrows
    gapminder,
    countrycode
)

#' # Basic choropleth map
# basic choropleth map #-------------
#' ## Data
## data #-----------
world_countries <- rnaturalearth::ne_countries(scale = 'medium',
                                               returnclass = 'sf') %>% 
    tibble()

names(world_countries)

(wdf_plot <- world_countries %>% 
        select(admin, pop_year, pop_est, geometry) %>% 
        st_as_sf(crs = st_crs(4326)))

wdf_plot %>% count(pop_year)

#' ## Plot
## plot #---------------------------
#' Basic map
fig_basis <- wdf_plot %>% 
    filter(pop_year == 2019) %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry,
                fill = pop_est),
            color = "white",
            linewidth = 0.1) +
    labs(title = "Population (2019)",
         fill = NULL) + 
    theme_minimal()

fig_basis

#' Customize colors
fig_basis +
    scale_fill_viridis_c(option = "rocket",
                         # reverse palette
                         direction = -1,
                         # color for missing values
                         na.value = "grey80")

fig1 <- fig_basis +
    scale_fill_viridis_c(option = "rocket",
                         # reverse palette
                         direction = -1,
                         # color for missing values
                         na.value = "grey80",
                         # log10 transformation
                         trans = "log10",
                         # labels for log scale
                         labels = scales::label_log(digits = 2)) +
    theme(legend.position = "bottom",
          legend.key.width = grid::unit(1.5, "cm"))

fig1

#' Adding map elements
fig2 <- fig1 +
    # add scale bar
    ggspatial::annotation_scale(location = "bl",
                                width_hint = 0.3,
                                style = "ticks") +
    # add north arrow
    ggspatial::annotation_north_arrow(location = "tr",
                                      which_north = "true",
                                      pad_x = unit(0.1, "in"),
                                      pad_y = unit(1, "in"),
                                      style = ggspatial::north_arrow_fancy_orienteering) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.key.width = grid::unit(1.5, "cm"))

fig2

#' # Mapping African Indicators
# african indicators #--------------------
#' ## Prepare data
## prepare data #--------------------------
#' Get African country polygons
(africa_sf <- rnaturalearth::ne_countries(scale = "medium",
                                         continent = "Africa",
                                         returnclass = "sf") %>% 
    dplyr::select(iso_a3 = adm0_a3, name, geometry))

#' Filter `gapminder` for latest year
(gapminder_latest <- gapminder %>% 
    slice_max(year) %>% 
    select(country, lifeExp))

#' Add iso_a3 codes to gapminder_latest
gapminder_latest %>% 
    mutate(iso_a3 = countrycode::countrycode(country,
                                             origin = 'country.name',
                                             destination = 'iso3c'))

#' Add life expectancy to polygons
(wdf_africa <- gapminder_latest %>% 
    mutate(iso_a3 = countrycode::countrycode(country,
                                             origin = 'country.name',
                                             destination = 'iso3c')) %>% 
    right_join(africa_sf,
               by = join_by(iso_a3)) %>% 
    select(iso_a3, lifeExp, country, name, geometry))

#' ## Plot
## plot #--------------------------
wdf_africa %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,
                fill = lifeExp),
            color = "white",
            linewidth = 0.1) +
    scale_fill_viridis_c(option = "viridis",
                         direction = -1,
                         name = "Life Exp.",
                         na.value = "grey90") +
    labs(title = paste("African Life Expectancy (2007)"),
         caption = "Data: Gapminder & Natural Earth") +
    theme_void() +
    theme(legend.position.inside = c(0.15, 0.8),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(hjust = 0.95, vjust = 30))

