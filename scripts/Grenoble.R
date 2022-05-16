# Notes -------------------------------------------------------------------

# Format 10-15

# Initialisation ----------------------------------------------------------

rm(list = ls()) # better to restart session as well

source("R/functions.R")

colour_street <- "#ffbe7f"
colour_water <- "#5A97AA"
colour_background <- "#282828"

# Processing --------------------------------------------------------------

# coordinates <- getbb("Grenoble France")
coordinates <- make_coordinates(c(5.68, 5.753),
                                c(45.146 , 45.223))

roads <- get_osmdata(coordinates,
                     key = "highway", 
                     value = c("motorway", "trunk", "primary",
                               "motorway_link", "trunk_link", "primary_link"))

streets <- c(
  get_osmdata(coordinates,
              key = "highway", 
              value = c("secondary", "tertiary",
                        "secondary_link", "tertiary_link")),
  get_osmdata(coordinates,
              key = "junction", 
              value = c("roundabout"))
)

small_streets <- get_osmdata(coordinates,
                             key = "highway", 
                             value = c("residential", "living_street", "mini_roundabout"))

other_streets <- get_osmdata(coordinates,
                             key = "highway", 
                             value = c("service", "footway", "pedestrian"))

river <- get_osmdata(coordinates,
                     key = "waterway",
                     value = "river")

# Map ---------------------------------------------------------------------

ggplot() +
  # Streets (polygons); put before green spaces as some roundabout have grass in the middle
  geom_sf(data = roads$osm_polygons,
          inherit.aes = FALSE,
          color = colour_street, fill = colour_street,
          size = .1) +
  geom_sf(data = streets$osm_polygons,
          inherit.aes = FALSE,
          color = colour_street, fill = colour_street,
          size = .1) +
  # Water
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = colour_water,
          size = 2.5, alpha = 1) +
  # Streets (lines)
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .75) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .5) +
  geom_sf(data = other_streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .2) +
  # Options
  coord_sf(xlim = coordinates["x", ],
           ylim = coordinates["y", ],
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = colour_background),
        panel.background = element_rect(fill = colour_background))

if (FALSE) {
  ggsave(here("docs", paste0("Grenoble.jpg")),
         width = 20 * ratio_xy(coordinates), height = 20, units = "cm",
         scale = 1.1, dpi = 600)
}
