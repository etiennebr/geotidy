context("test-exports")


# point -------------------------------------------------------------------

test_that("st_point returns POINT", {
  x <- st_point(list(c(1 ,2)))
  expect_is(x, "sfc_POINT")
  expect_crs(x, sf::NA_crs_)
  expect_npoints(x, 1)
  expect_true(is_geometry(x))
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
  x <- tibble::tibble(
    g = c("a", "a", "b"),
    point = c(st_point(12, 21), st_point(21, 12), st_point(14, 15)))
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
  x <- tibble::tibble(
    g = c("a", "a"),
    point = c(st_point(12, 21), st_point(21, 12))
  ) %>%
    group_by(g) %>%
    summarise(line = st_makeline(point))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
})

test_that("Cannot create a line from a single point", {
  expect_error(st_makeline(st_point(1, 1)), "must contain at least 2 points")
})

test_that("A set of point is assembled as a line (no group)", {
  x <- tibble::tibble(
    g = c("a", "a"),
    point = c(st_point(12, 21), st_point(21, 12))
    ) %>%
    summarise(line = st_makeline(point))
  # TODO: st_union seems to duplicate lines
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_equal(nrow(x[["line"]][[1]]), 2)
})

test_that("A pair of points create a line", {
  x <- tibble::tibble(
    g = c("a", "a"),
    origin      = c(st_point(12, 21), st_point(21, 12)),
    destination = c(st_point(13, 22), st_point(22, 13))
  ) %>%
    mutate(line = st_makeline(origin, destination))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_equal(nrow(x[["line"]][[1]]), 2)
})

test_that("A pair of points create a line", {
  x <- tibble::tibble(
    origin      = c(st_point(12, 21), st_point(13, 21), st_point(14, 21)),
    destination = c(st_point(13, 22), st_point(14, 22), st_point(15, 22))
  ) %>%
    mutate(line = st_makeline(origin, destination))
  expect_is(x[["line"]], "sfc_LINESTRING")
  expect_is(x[["line"]], "sfc")
  expect_npoints(x$line, rep(2, 3))
})

test_that("Create line from multipoints", {
  x <- tibble::tibble(
    g = c("a", "a"),
    point = c(st_point(12, 21), st_point(21, 12))) %>%
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
  expect_npoints(x[["line"]], 2)

  list_of_points <- map2(1:10, 1:10, st_point)
  expect_npoints(st_makeline(list_of_points), 10)
})

test_that("Create line from list of coordinates", {
  x <- tibble(tuple = list(c(1,2), c(3, 4), c(5, 6))) %>%
    mutate(pts = st_makepoint(tuple)) %>%
    summarise(linestring = st_makeline(pts))
  expect_is(x[["linestring"]], "sfc_LINESTRING")
  expect_is(x[["linestring"]], "sfc")
  expect_npoints(x[["linestring"]], 3)
})

test_that("Create line from numeric", {
  expect_error(st_makeline(1:3), "second coordinate")
  expect_error(st_makeline(1:3, 1:2), "same length")
  expect_error(st_makeline(1:3, letters[1:3]), "numeric")
  # recycle `y`
  expect_identical(st_makeline(1:2, 1), st_makeline(list(st_point(1, 1), st_point(2, 1))))
  # recycle `x`
  expect_identical(st_makeline(1, 1:2), st_makeline(list(st_point(1, 1), st_point(1, 2))))
})

test_that("Create line from list of coordinates", {
  x <- tibble(x = 1:3, y = 1:3)
  y <- summarise(x, linestring = st_makeline(x, y))

  expect_is(y[["linestring"]], "sfc_LINESTRING")
  expect_is(y[["linestring"]], "sfc")
  expect_npoints(y[["linestring"]], 3)
  expect_error(summarise(x, linestring = st_makeline(x)), "second coordinate")
  expect_error(summarise(x, linestring = st_makeline(x, "a")), "numeric")
  expect_error(summarise(x, linestring = st_makeline(1:3, 1:2)), "same length")
})
# dump --------------------------------------------------------------------

test_that("st_dumppoints returns a nested tibble", {
  x <- tibble(tuple = list(c(1,2), c(3, 4), c(5, 6))) %>%
    mutate(pts = st_makepoint(tuple))
  y <- x %>%
    summarise(linestring = st_makeline(pts)) %>%
    mutate(pts = st_dumppoints(linestring))
  expect_is(y$pts, "list")
  expect_is(y$pts[[1]], "tbl_df")
  expect_equal(y$pts[[1]]$geom, x$pts)
  expect_equal(y$pts[[1]]$path, seq_len(nrow(x)))
})

test_that("st_dumppoints returns a nested tibble", {
  x <- tibble(tuple = list(c(1,2), c(3, 4), c(5, 6))) %>%
    add_row(tuple = list(c(7, 8))) %>%
    group_by(g = c(1, 1, 2, 2)) %>%
    mutate(pts = st_makepoint(tuple))
  y <- x %>%
    summarise(linestring = st_makeline(pts)) %>%
    mutate(pts = st_dumppoints(linestring))
  expect_is(y$pts, "list")
  expect_is(y$pts[[1]], "tbl_df")
  expect_equal(y$pts[[1]]$geom, x$pts[1:2])
  expect_equal(y$pts[[1]]$path, 1:2)
})

# Coordinates -----------------------------------------------------------------
test_that("st_coordinates returns a nested tibble", {
  x <- tibble(tuple = list(c(1,2), c(3, 4), c(5, 6))) %>%
    mutate(pts = st_makepoint(tuple))
  y <- x %>%
    summarise(linestring = st_makeline(pts)) %>%
    mutate(coords = st_coordinates(linestring))
  expect_is(y$coords, "list")
  expect_is(y$coords[[1]], "tbl_df")
})

test_that("st_coordinates returns a `.path` for each point", {
  x <- st_makepoint(list(c(1,2), c(3, 4), c(5, 6)))

  expect_equal(names(st_coordinates(x)[[1]]), c(".x", ".y", ".path"))
  x <- st_multi(x)
  expect_equal(names(st_coordinates(x)[[1]]), c(".x", ".y", ".l1", ".path"))
  x <- st_makeline(x)
  expect_equal(names(st_coordinates(x)[[1]]), c(".x", ".y", ".l1", ".path"))
  # todo: add other types
})
