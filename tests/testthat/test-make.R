context("test-test-exports")

test_that("st_point returns POINT", {
  x <- st_point(list(c(1 ,2)))
  expect_is(x, "sfc_POINT")
})

test_that("st_point works on columns", {
  x <- tibble::tibble(x = 1:2, y = 2:1) %>%
    mutate(geometry = st_point(x, y))
  expect_is(x, "tbl_df")
  expect_is(x[["geometry"]], "sfc_POINT")

  # single row
  x <- tibble::tibble(x = 12, y = 21) %>%
    mutate(geometry = st_point(x, y))
  expect_is(x, "tbl_df")
  expect_is(x[["geometry"]], "sfc_POINT")
})

test_that("st_point works with single columns", {
  v <- purrr::map2(1:2, 2:1, c)
  x <- st_point(v)
  expect_is(x, "sfc")
  expect_equal(x, st_point(1:2, 2:1))
  expect_is(x, "sfc_POINT")
})
