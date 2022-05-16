# Notes -------------------------------------------------------------------

# There is a minor problem that some water (river) is marked as polygon when it is actually land
# but if we don't use water as polygon, then locations where there is water (and which are only marked as water) disappear
# for the moment, only plot polygon water for location with names
# this drops small patches of water but it is acceptable

# Initialisation ----------------------------------------------------------

rm(list = ls()) # better to restart session as well

source("R/functions.R")
library(extrafont)
loadfonts(device = "win")

colour_street <- "#ffbe7f"
colour_green <- "#477725"
colour_water <- "#5A97AA"
colour_background <- "#282828"

format <- "4_3"
format <- match.arg(format, c("24_30", "15_21", "4_3"))

# Processing --------------------------------------------------------------

# coordinates <- getbb("Rennes France")

if (format == "24_30") {
  coordinates <- make_coordinates(c(-1.7325, -1.5960),
                                  c(48.0750, 48.1479))
} else if (format == "15_21") {
  coordinates <- make_coordinates(c(-1.738, -1.585),
                                  c(48.0750, 48.1479))
} else if (format == "4_3") {
  coordinates <- make_coordinates(c(-1.731, -1.586),
                                  c(48.0748, 48.1474))
}

roads <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary",
                            "motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

streets <- c(
  coordinates %>%
    opq() %>%
    add_osm_feature(key = "highway", 
                    value = c("secondary", "tertiary",
                              "secondary_link", "tertiary_link")) %>%
    osmdata_sf(),
  coordinates %>%
    opq() %>%
    add_osm_feature(key = "junction", 
                    value = c("roundabout")) %>%
    osmdata_sf()
)

small_streets <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street", "mini_roundabout")) %>%
  osmdata_sf()

other_streets <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("unclassified", "service", "footway", "pedestrian")) %>%
  osmdata_sf()

river <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

stream <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = c("stream")) %>%
  osmdata_sf()

water <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "water", value = c("pond", "lake", "reflecting_pool", "reservoir", "basin")) %>%
  osmdata_sf()

water2 <- coordinates %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()

green_spaces <- c(
  coordinates %>%
    opq() %>%
    add_osm_feature(key = "leisure", value = c("park", "garden", "village_green", "common", "pitch")) %>%
    osmdata_sf(),
  coordinates %>%
    opq() %>%
    add_osm_feature(key = "landuse", value = c("grass", "recreation_ground", "forest")) %>% # farmland
    osmdata_sf()
)

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
  # Green spaces
  geom_sf(data = green_spaces$osm_polygons,
          inherit.aes = FALSE,
          colour = colour_green, fill = colour_green,
          size = .1, alpha = .9) +
  # Water (lines)
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = colour_water,
          size = 1, alpha = .6) +
  geom_sf(data = stream$osm_lines,
          inherit.aes = FALSE,
          color = colour_water,
          size = .5, alpha = .6) +
  # Water (polygons)
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = FALSE,
          colour = colour_water, fill = colour_water,
          size = .1, alpha = .9) +
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          colour = colour_water, fill = colour_water,
          size = .1, alpha = .9) +
  geom_sf(data = water2$osm_multipolygons,
          inherit.aes = FALSE,
          colour = colour_water, fill = colour_water,
          size = .1, alpha = .9) +
  geom_sf(data = water2$osm_polygons %>% drop_na(name),
          inherit.aes = FALSE,
          colour = colour_water, fill = colour_water,
          size = .1, alpha = .9) +
  # Streets (lines)
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .3, alpha = .3) +
  geom_sf(data = other_streets$osm_lines,
          inherit.aes = FALSE,
          color = colour_street,
          size = .15) +
  geom_sf(data = small_streets$osm_polygons, # cf. circular streets, fill with background
          inherit.aes = FALSE,
          color = colour_street, fill = colour_background,
          size = .15) + 
  # Signature
  annotate("label",
           x = sum(coordinates["x", ] * c(.035, .965)),
           y = sum(coordinates["y", ] * c(.965, .035)),
           label = "G H",
           fontface = "bold.italic",
           family = "Palatino Linotype",
           colour = colour_street,
           fill = colour_background,
           size = 2.5) +
  # Options
  coord_sf(xlim = coordinates["x", ],
           ylim = coordinates["y", ],
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = colour_background),
        panel.background = element_rect(fill = colour_background))

if (FALSE) {
  ggsave(paste0("Maps/Rennes_", format, ".jpg"),
         width = 20 * ratio_xy(coordinates), height = 20, units = "cm",
         scale = 1.1, dpi = 600)
}
