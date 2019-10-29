context("test-tibble")

test_that("tibble is loaded with geotidy", {
  expect_silent(x <- tibble(x = 1))
})

test_that("dplyr is loaded with geotidy", {
  expect_silent(mutate(data.frame(x = 1), y = x + 1))
})

test_that("can pipe", {
  expect_silent(1 %>% sum())
})

test_that("can bind rows", {
  x <- tibble::tibble(v = 1, point = st_point(12, 21) %>% st_set_crs(4326))
  y <- tibble::tibble(v = 3, point = st_point(21, 11) %>% st_set_crs(4326))

  z <- dplyr::bind_rows(x, y)
  expect_equal(nrow(z), 2)
  expect_equal(sf::st_crs(z), 4326)
})
