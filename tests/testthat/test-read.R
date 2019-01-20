context("test-read")

test_that("reads a `tibble`", {
  x <- st_read(system.file("shape/nc.shp", package="sf"))
  expect_s3_class(x, "tbl_df")
})
