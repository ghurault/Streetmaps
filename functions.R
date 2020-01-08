make_coordinates <- function(x, y) {
  # Generate coordinates matrix
  #
  # Args:
  # x: vector of longitudes of the bottom left corner and the top right corner
  # y: vector of latitudes of the bottom left corner and the top right corner
  #
  # Returns:
  # Matrix
  
  matrix(c(x, y),
         nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("x", "y"), c("min", "max")))
}

ratio_xy <- function(coordinates) {
  # Compute ratio of distance covered by the x-axis by the distance covered by the y-axis
  #
  # Args:
  # coordinates: Matrix of coordinates
  #
  # Returns:
  # Numeric ratio
  
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