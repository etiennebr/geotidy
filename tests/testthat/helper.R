# expect given number of points in geometry
expect_npoints <- function(object, expected, ...) {
  expect_equal(st_npoints(object), expected, ...)
}

expect_crs <- function(object, expected, ...) {
  expect_equal(sf::st_crs(object), sf::st_crs(expected))
}
