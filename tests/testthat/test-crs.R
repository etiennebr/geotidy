context("test-crs")

test_that("is_crs", {
  expect_true(is_crs(sf::st_crs(4326)))
  expect_false(is_crs(st_point(1, 2) %>% st_set_crs(4326)))
})

test_that("map_crs", {
  expect_crs(map_crs(replicate(2, st_point(1, 2) %>% st_set_crs(4326))), sf::NA_crs_)
  expect_equal(
    map_crs(c(replicate(2, st_point(1, 2) %>% st_setsrid(4326)), 4326)),
    list(sf::NA_crs_, sf::NA_crs_, sf::st_crs(4326))
  )
  expect_error(
    map_crs(c(replicate(2, st_point(1, 2) %>% st_setsrid(4326)), "4326")), "coerce")
  crs <- replicate(2, sf::st_crs(4326))
  expect_equal(unique_crs(crs), sf::st_crs(4326))
})

test_that("unique_crs stops when multiple crs", {
  pts <- list(st_point(1, 2) %>% st_setsrid(4326), st_point(1, 2))
  expect_error(unique_crs(pts), "same crs")
})
