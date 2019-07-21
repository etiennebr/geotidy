context("test-st_countn")

test_that("can count number of points", {
  expect_npoints(st_makepoint(1, 2), 1)
  expect_npoints(st_makepoint(list(1:2, 3:4)), c(1, 1))
  expect_npoints(st_makepoint(list(1:2, 3:4)) %>% st_multi(), 2)
  expect_npoints(st_makepoint(list(1:2, 3:4)) %>% st_makeline(), 2)
})

test_that("can count number of points of multi polygons", {
  x <- st_geomfromtext(
    "MULTIPOLYGON(((77 29,72 26,77 21,77 29), (17 19,12 16,15 15,17 11,17 19)))"
    )
  expect_equal(st_numpoints(x), 9)
  expect_equal(st_numpoints(c(x, x)), c(9, 9))
})
