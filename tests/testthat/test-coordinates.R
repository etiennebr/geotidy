context("test-coordinates")

test_that("st_coordinates work", {
  x <- st_makeline(c(11, 12), c(13, 14))
  y <- tibble::tibble(.x = c(11, 12), .y = c(13, 14), .l1 = 1, .path = 1:2)
  expect_identical(st_coordinates(x), list(y))
})
