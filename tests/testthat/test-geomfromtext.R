context("wkt")

test_that("Can convert from text", {
  x <- st_geomfromtext('POLYGON((38 16,38 50,65 50,66 16,38 16))')
  expect_is(x, "sfc")
  expect_is(x, "sfc_POLYGON")
  expect_npoints(x, 5)

  x <- st_geomfromtext(c('POLYGON((38 16,38 50,65 50,66 16,38 16))', 'POLYGON((35 13,22 51,64 49,35 13))'))
  expect_is(x, "sfc")
  expect_is(x, "sfc_POLYGON")
  expect_npoints(x, c(5, 4))

  expect_error(st_geomfromtext("POINT('A' 'B')"), "OGR error")
  expect_error(st_geomfromtext(3), "x is not a character vector")

  # Refused by postgis >= 2
  # expect_error(st_geomfromtext('GEOMETRYCOLLECTION(EMPTY)'))
  expect_equal(
    st_geomfromtext('GEOMETRYCOLLECTION EMPTY')[[1]],
    structure(list(), class = c("XY", "GEOMETRYCOLLECTION", "sfg"))
    )
})

test_that("Can convert from EWKT", {
  x <- st_geomfromtext('SRID=4326;LINESTRING(-71.1602 42.2587,-71.1608 42.2591,-71.1611 42.2593)')
  expect_is(x, "sfc")
  expect_is(x, "sfc_LINESTRING")
  expect_npoints(x, 3)
  expect_crs(x, 4326)
})
