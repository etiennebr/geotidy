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
