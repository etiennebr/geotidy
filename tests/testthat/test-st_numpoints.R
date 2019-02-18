context("test-st_countn")

test_that("can count number of points", {
  expect_npoints(st_makepoint(1, 2), 1)
  expect_npoints(st_makepoint(list(1:2, 3:4)), c(1, 1))
  expect_npoints(st_makepoint(list(1:2, 3:4)) %>% st_multi(), 2)
  expect_npoints(st_makepoint(list(1:2, 3:4)) %>% st_makeline(), 2)
})
