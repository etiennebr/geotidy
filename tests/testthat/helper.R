# expect given number of points in geometry
expect_npoints <- function(object, expected, ...) {
  expect_equal(st_npoints(object), expected, ...)
}
