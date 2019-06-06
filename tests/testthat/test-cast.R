context("test-cast")

test_that("`cast_union` returns union of vectors", {
  pts <- st_makepoint(c(1:3), c(3:5))
  expect_equal(cast_union(pts, pts, .cast = "LINESTRING"), st_makeline(pts))
})

test_that("`cast_combine` works", {
  pts <- st_makepoint(c(1:3), c(3:5))
  expect_equal(cast_combine(pts, .cast = "LINESTRING"), st_makeline(pts))
  expect_error(cast_combine(pts, .cast = "LINESTRING", .by_feature = TRUE), "must provide `y`")
})
