# Notes -------------------------------------------------------------------

# I don't consider water/river because there shouldn't be any

# Initialisation ----------------------------------------------------------

rm(list = ls()) # better to restart session as well

source("R/functions.R")

colour_street <- "black"
colour_green <- "#477725"
colour_water <- "#5A97AA"
colour_background <- "white"

# Processing --------------------------------------------------------------

# coordinates <- getbb("Mons-en-Baroeul France")
coordinates <- make_coordinates(c(3.09, 3.13),
                                c(50.628,50.653)) # c(50.634,50.653)

roads <- get_osmdata(coordinates,
                     key = "highway", 
                     value = c("motorway", "trunk", "primary",
                               "motorway_link", "trunk_link", "primary_link"))

rail <- get_osmdata(coordinates,
                    key = "railway", 
                    value = c("rail"))

streets <- c(
  get_osmdata(coordinates,
              key = "highway", 
              value = c("secondary", "tertiary",
                        "secondary_link", "tertiary_link")),
  get_osmdata(coordinates,
              key = "junction", 
              value = c("roundabout"))
)

small_streets <- c(
  get_osmdata(coordinates,
              key = "highway", 
              value = c("residential", "living_street", "mini_roundabout")),
  get_osmdata(coordinates,
              key = "junction", 
              value = c("roundabout"))
)

other_streets <- get_osmdata(coordinates,
                             key = "highway", 
                             value = c("unclassified", "service", "footway", "pesdestrian",
                                       "track", "path", "bridleway"))

green_spaces <- c(
  get_osmdata(coordinates,
              key = "leisure",
              value = c("park", "garden", "village_green", "common", "pitch")),
  get_osmdata(coordinates,
              key = "landuse",
              value = c("grass", "recreation_ground")), # # forest, farmland
  get_osmdata(coordinates,
              key = "natural",
              value = c("wood"))
)

# Map ---------------------------------------------------------------------

p <- ggplot() +
  # Streets polygons
  geom_sf(data = roads$osm_polygons,
          inherit.aes = FALSE,
          color = colour_street, fill = colour_street,
          size = .1) +
  geom_sf(data = streets$osm_polygons,
          inherit.aes = FALSE,
          color = colour_street, fill = colour_street,
          size = .1) +
  geom_sf(data = small_streets$osm_polygons,
          inherit.aes = FALSE,
          color = colour_street, fill = colour_street,
          size = .1) +
  # Green space
  geom_sf(data = green_spaces$osm_polygons,
          inherit.aes = FALSE,
          colour = colour_green, fill = colour_green,
          size = .1, alpha = .9) +
  geom_sf(data = green_spaces$osm_multipolygons,
          inherit.aes = FALSE,
          colour = colour_green, fill = colour_green,
          size = .1, alpha = .9) +
  # Rail
  geom_sf(data = rail$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = 1) +
  # Streets
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = 1) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .5) +
  geom_sf(data = other_streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .3) +
  # Options
  coord_sf(xlim = NULL, ylim = NULL) +
  # coord_sf(xlim = coordinates["x", ],
  #          ylim = coordinates["y", ],
  #          expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = colour_background))

p

# Recompute coordinates of plot when coord_sf limits are NULL
rx <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
ry <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
r <- ratio_xy(make_coordinates(rx, ry))

if (FALSE) {
  ggsave(here("docs", "Mons.jpg"),
         width = 20 * r, height = 20, units = "cm",
         scale = 1, dpi = 600)
}
