test_that("st_envelope can create a polygon with crs", {
  geom <- st_makeline(1:10, 11:20) %>%
    sf::st_set_crs(4326)
  expect_equal(
    st_envelope(geom),
    st_geomfromtext("SRID=4326;POLYGON ((1 11, 10 11, 10 20, 1 20, 1 11))")
  )
})

test_that("st_envelope does not aggregate by default", {
  x <- tibble(
    line = c(
      st_makeline(1:10, 11:20),
      st_makeline(1:10+2, 11:20 -2)
    ) %>%
      sf::st_set_crs(4326)
  )

  expect_equal(
    st_envelope(x$line),
    c(
      st_geomfromtext("SRID=4326;POLYGON ((1 11, 10 11, 10 20, 1 20, 1 11))"),
      st_geomfromtext("SRID=4326;POLYGON ((3 9, 12 9, 12 18, 3 18, 3 9))")
    )
  )

  # aggregate with group_by
  y <- summarise(x, envelope = sf::st_combine(line) %>% st_envelope())

  expect_equal(
    y$envelope,
    st_geomfromtext("SRID=4326;POLYGON ((1 9, 12 9, 12 20, 1 20, 1 9))")
  )
})
