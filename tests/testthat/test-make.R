context("test-test-exports")


# point -------------------------------------------------------------------

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


# multipoints -------------------------------------------------------------

test_that("st_multi on point returns multipoint", {
  x <- tibble::tibble(point = st_point(12, 21))
  x <- mutate(x, multi = st_multi(point))
  expect_is(x[["multi"]], "sfc_MULTIPOINT")
  expect_is(x[["multi"]], "sfc")
})

test_that("st_multi on points returns multipoints", {
  x <- tibble::tibble(g = c("a", "a", "b"), point = c(st_point(12, 21), st_point(21, 12), st_point(14, 15)))
  x <-
    x %>%
    group_by(g) %>%
    summarise(multi = st_multi(point))
  expect_is(x[["multi"]], "sfc_MULTIPOINT")
  expect_is(x[["multi"]], "sfc")
})


test_that("If the geometry is already a `MULTI*`, it is returned unchanged.", {
  x <- tibble::tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12))) %>%
    group_by(g) %>%
    summarise(multi = st_multi(point)) %>%
    mutate(multi2 = st_multi(multi))
  expect_is(x[["multi"]], "sfc_MULTIPOINT")
  expect_is(x[["multi"]], "sfc")
})
# line --------------------------------------------------------------------

test_that("A set of point is assembled as a line", {
  x <- tibble::tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12))) %>%
    group_by(g) %>%
    summarise(line = st_makeline(point))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
})

test_that("A set of point is assembled as a line", {
  x <- tibble::tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12))) %>%
    summarise(line = st_makeline(point))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_equal(nrow(x[["line"]][[1]]), 2)
})

test_that("Create line from multipoints", {
  x <- tibble::tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12))) %>%
    summarise(multi = st_multi(point)) %>%
    mutate(line = st_makeline(multi))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_equal(nrow(x[["line"]][[1]]), 2)
})

test_that("Create line from list of points (summarise)", {
  x <- tibble::tibble(points = c(st_point(12, 21), st_point(21, 12))) %>%
    summarise(line = st_makeline(points))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_equal(nrow(x[["line"]][[1]]), 2)

  list_of_points <- map2(sample(1:10), sample(1:10), st_point)
  st_makeline(list_of_points)
})

test_that("Create line from list of coordinates", {
  x <- tibble(tuple = list(c(1,2), c(3, 4), c(5, 6))) %>%
    mutate(pts = st_makepoint(tuple)) %>%
    summarise(linestring = st_makeline(pts))
  expect_is(x[["linestring"]], "sfc_LINESTRING")
  expect_is(x[["linestring"]], "sfc")
  expect_npoints(x[["linestring"]], 3)
})


# dump --------------------------------------------------------------------

test_that("st_dump returns a nested tibble", {
  expect_true(TRUE)
})
