context("test-cast")

test_that("`cast_union` returns union of vectors", {
  pts <- st_makepoint(c(1:3), c(3:5))
  expect_equal(cast_union(pts, pts, "LINESTRING"), st_makeline(pts))
})

test_that("`cast_combine` works", {
  pts <- st_makepoint(c(1:3), c(3:5))
  expect_equal(cast_combine(pts, "LINESTRING"), st_makeline(pts))
})
