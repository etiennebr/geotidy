context("test-map")

test_that("map_sfc returns an `sfc`", {
  x <- list(1:2, 2:3)
  y <- map_sfc(x, ~sf::st_point(.x))
  expect_is(y, "sfc")
})
