# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(osmdata)
library(sf)

# Functions ---------------------------------------------------------------

#' Generate coordinates matrix
#'
#' @param x Vector of longitudes of the bottom left corner and the top right corner
#' @param y Vector of latitudes of the bottom left corner and the top right corner
#'
#' @return Matrix
make_coordinates <- function(x, y) {
  matrix(c(x, y),
         nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("x", "y"), c("min", "max")))
}

#' Compute ratio of distance covered by the x-axis by the distance covered by the y-axis
#'
#' @param coordinates Matrix of coordinates
#'
#' @return Numeric ratio
ratio_xy <- function(coordinates) {
  
  library(geosphere)
  
  mu_x <- mean(coordinates["x", ])
  mu_y <- mean(coordinates["y", ])
  
  dist_x <- distm(c(coordinates["x", "min"], mu_y),
                  c(coordinates["x", "max"], mu_y),
                  fun = distHaversine)
  dist_y <- distm(c(mu_x, coordinates["y", "min"]),
                  c(mu_x, coordinates["y", "max"]),
                  fun = distHaversine)
  
  return(dist_x / dist_y)
}