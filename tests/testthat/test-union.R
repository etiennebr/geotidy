context("test-union")

# reference database test script
# create temp table geom as (select 1 as n, 'POINT(1 2)'::geometry as g);
# insert into geom (SELECT 2, 'point(3 4)'::geometry);
# insert into geom (SELECT 3, 'point(5 6)'::geometry);
# select n, st_astext(st_union(geometry)) from geom group by n;
# select n, st_astext(st_union(g)) from geom;

geom <- tibble(
  x = 1:3,
  y = 4:6,
  origin = st_point(x, y),
  destin = st_point(x + 1, y + 1)
)

test_that("When provided only x, aggregation happens on x", {
  geom_union <- st_union(geom$origin)
  expect_length(geom_union, 1)
  expect_npoints(geom_union, 3)
  expect_is(geom_union, "sfc_MULTIPOINT")

  geom_union <- summarise(geom, union = st_union(origin))
  expect_length(geom_union$union, 1)
  expect_npoints(geom_union$union, 3)
  expect_is(geom_union$union, "sfc_MULTIPOINT")
})

test_that("When provided x and y, aggregation happens pair-wise", {
  geom_union <- mutate(geom, union = st_union(origin, destin))
  expect_length(geom_union$union, 3)
  expect_npoints(geom_union$union, c(2, 2, 2))
  expect_is(geom_union$union, "sfc_MULTIPOINT")
})

test_that("cannot union two vectors of different length, except if length one", {
  expect_error(st_union(geom$origin[1:3], geom$origin[1:2]), "consistent lengths")
  expect_length(st_union(geom$origin[1], geom$origin[1:3]), 3)
  expect_length(st_union(geom$origin[1:3], geom$origin[1]), 3)
})
