context("test-union")

test_that("can union a vector", {
  geom <- tibble(
    x = 1:3,
    y = 4:6,
    origin = st_point(x, y),
    destin = st_point(x + 1, y + 1)
    )
  geom <- mutate(geom, union = st_union(origin))
  expect_length(geom$union, 3)
  expect_is(geom$union, "sfc_POINT")
  geom <- summarise(geom, union = st_union(origin, by_feature = FALSE))
  expect_length(geom$union, 1)
  expect_is(geom$union, "sfc_MULTIPOINT")
})

test_that("can union two vectors", {
  geom <- tibble(
    x = 1:3,
    y = 4:6,
    origin = st_point(x, y),
    destin = st_point(x + 1, y + 1)
  )
  geom <- mutate(geom, union = st_union(origin, destin))
  expect_length(geom$union, 3)
  expect_is(geom$union, "sfc_MULTIPOINT")
  geom <- summarise(geom, union = st_union(origin, destin, by_feature = FALSE))
  expect_length(geom$union, 1)
  expect_is(geom$union, "sfc_MULTIPOINT")
})

test_that("cannot union two vectors of different length", {
  expect_error(st_union(1:2, 1:6), "same length")
})

test_that("identical geometries are unioned", {
  geom <- tibble(
    x = 1:3,
    y = 4:6,
    origin = st_point(x, y),
    destin = st_point(x, y)
  )
  sumgeom <- mutate(geom, union = st_union(origin, destin))
  expect_length(sumgeom$union, 3)
  expect_is(sumgeom$union, "sfc_POINT")
  sumgeom <- summarise(geom, union = st_union(origin, destin, by_feature = FALSE))
  expect_length(sumgeom$union, 1)
  expect_is(sumgeom$union, "sfc_MULTIPOINT")
  geom2 <- mutate(
    geom,
    x = 1,
    y = 1,
    origin = st_point(x, y)
  )
  sumgeom <- summarise(geom2, union = st_union(origin, origin, by_feature = FALSE))
  expect_length(sumgeom$union, 1)
  expect_identical(sumgeom$union, st_point(1,1))
})

