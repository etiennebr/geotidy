context("test-st_dwithin")

x <- tibble::tibble(x = 1:3, y = 3:1) %>%
  mutate(geometry = st_point(x, y))

test_that("Returns true if geometry is within distance", {
  expect_equal(st_dwithin(x$geometry, x$geometry, 1), rep(TRUE, nrow(x)))
  expect_equal(st_dwithin(x$geometry, x$geometry[1], 2), c(TRUE, TRUE, FALSE))
})

test_that("st_dwithin does not do cartesian join for vectors of inequal length", {
  # postgis does a cartesian join, but doing so would break our rule of not recycling
  # vectors of length > 1. It is still possible to mimic the behavior by joining
  # the data before with `dplyr`.
  expect_error(st_dwithin(x$geometry, x$geometry[1:2], 3), "recycle")
})

test_that("st_dwithin requires two sets of geometries", {
  expect_error(st_dwithin(x$geometry, distance = 3), "requires two")
})
