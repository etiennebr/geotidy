
<!-- README.md is generated from README.Rmd. Please edit that file -->
geotidy
=======

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/etiennebr/geotidy.svg?branch=master)](https://travis-ci.org/etiennebr/geotidy) [![Codecov test coverage](https://codecov.io/gh/etiennebr/geotidy/branch/master/graph/badge.svg)](https://codecov.io/gh/etiennebr/geotidy?branch=master) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) <!-- badges: end -->

Manipulate spatial data tidily. `Geotidy` provides a selection of spatial functions from the `sf` package that are adapted to a tidy workflow. It relies on tibbles and `dplyr` verbs, and forces you to be explicit about the spatial operations you desire. The package is experimental. If you want to learn more about the motivations behind it, see the [Motivation](#motivation) section.

Installation
------------

You can install the experimental version of geotidy from github :

``` r
# install.packages("remotes")
remotes::install_github("etiennebr/geotidy")
```

Example
-------

Here's how you manipulate spatial data with `geotidy`.

``` r
library(tibble)
library(dplyr)
library(geotidy)

tibble(place = "Sunset Room", longitude = -97.7404985, latitude = 30.2645315) %>% 
  mutate(geometry = st_point(longitude, latitude))
#> # A tibble: 1 x 4
#>   place       longitude latitude            geometry
#>   <chr>           <dbl>    <dbl>             <POINT>
#> 1 Sunset Room     -97.7     30.3 (-97.7405 30.26453)
```

Motivation
==========

`geotidy` does less than `sf` to make manipulations explicit and compatible with other backends. *Explicit* means that it won't try to guess which column is a geometry and should receive the operation. It also makes it clear by reading the code, which geometry is impacted. This is done by treating geometry columns just like other tibble columns. `sf` often hides the geometry column, `geotidy` treats it just like a regular columns. This also makes it easier to interact with other OGC compliant tools, such as `postgis` or `spark+geomesa`.

Example
-------

While `sf` will guess which column should be buffered:

``` r
shp <- sf::st_read("")
st_buffer(shp)
```

`geotidy` forces to be explicit and use `dplyr` verbs

``` r
shp %>% 
  mutate(geometry = st_buffer(geometry))
```

If you already use `dplyr` with `sf`, `geotidy` should fell natural and remove some of the casting operations. `geotidy` guarantees that your data will stay tidy from start to finish. By having explicit management of geometry columns, it is easy to track multiple columns.

`geotidy` also guarantees that the returned values are either scalar, or a vector or a list with the same length than the original geometry and not drop any data without the user consent (looking at you `st_cast`!).

`geotidy` is not a fork or a separation from `sf`. It just adds a constrained layer on top of `sf` to facilitate a tidy workflow. It is an experiment and is very likely to change.
