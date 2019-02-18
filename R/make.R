# all direct exports of sfc methods from sf
#
# sfc <- sloop::s3_methods_class("sfc")
# sfc %>%
# mutate(generic = glue::glue())

#' Create points
#'
#' Create points from coordinates. `st_point` is simply a wrapper around `st_makepoint`.
#'
#' `.x` amd `.y` must be of the same length. If one of the coordiantes are
#' missing, it creates and empty point geometry.
#'
#' @return List column of points with the same length than `.x`` and `.y`.
#'
#' @rdname st_point
#' @param .x Numeric vector of x coordinates (e.g. longitude) or a list of pairs of coordinates. In the latter than case `.y`` can be missing.
#' @param .y Numeric vector of y coordinates (e.g. latitude).
#' @param crs Integer or character that identifies the coordinate reference system (default is `NA`). This can also be set by using [st_crs()] (or to find out more about `crs`). Coordinate reference system (`crs`) is the equivalent of POSTGIS's spatial reference identifier (`srid`).
#'
#' @family make
#' @export
#' @seealso [sf::st_point]
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' df <- tibble(longitude = -71.1043, latitude = 42.3150)
#' df %>%
#'   mutate(geometry = st_point(longitude, latitude))
st_makepoint <- function(.x, .y, crs = sf::NA_crs_) {
  if (base::missing(.y)) {
    pts <- purrr::map(.x, sf::st_point)
  } else {
    pts <- purrr::map2(.x, .y, ~sf::st_point(c(.x, .y)))
  }
  sf::st_as_sfc(pts, crs = crs)
}

#' @rdname st_point
#' @inheritParams st_makepoint
#' @export
st_point <- st_makepoint

#' Aggregate geometries
#'
#' Returns the geometry as a `MULTI*` geometry. If the geometry is already a `MULTI*`, it is returned unchanged.
#' @param .geom A geometry or a set of geometries of class `sfc` to be grouped. The geometries
#'   are often aggregated using a [dplyr::group_by()] followed by a
#'   [dplyr::summarise()].
#' @param ... Unused.
#' @return a single `MULTI*` geometry.
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12))) %>%
#'   group_by(g) %>%
#'   summarise(multi = st_multi(point))
#' @export
#' @family make
st_multi <- function(.geom, ...) UseMethod("st_multi")

#' @export
st_multi.sfc_POINT <- function(.geom, ...) {
  cast_union(.geom, .cast = "MULTIPOINT")
}

#' @export
st_multi.sfc_MULTIPOINT <- function(.geom, ...) {
  sf::st_union(.geom)
}

#' Creates a Linestring from point, multipoint, or line geometries.
#'
#' Lines can be created from point aggregates (using `summarise`) or from -- to
#' to create a 2-vertex line.
#' @param .geom A geometry or a set of geometries of class `sfc` to be converted
#'   to line. The geometries are often aggregated using a [dplyr::group_by()]
#'   followed by a[dplyr::summarise()].
#' @param .to A point geometry to create pairwise lines.
#' @param ... Unused.
#' @return A line (`LINESTRING`) of class `sfc`.
#' @export
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' x <- tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12)))
#' x %>%
#'  summarise(line = st_makeline(point))
#' @export
st_makeline <- function(.geom, .to, ...) UseMethod("st_makeline")

#' @export
st_makeline.sfc_POINT <- function(.geom, .to, ...) {
  if(!missing(.to)) {
    return(cast_union(.geom, .to, .cast = "LINESTRING", .by_feature = TRUE))
  }
  cast_union(.geom, .cast = "LINESTRING")
}

#' @export
st_makeline.sfc_MULTIPOINT <- function(.geom, ...) {
  # multipoints can be treated as points
  st_makeline.sfc_POINT(.geom)
}

#' @export
st_makeline.list <- function(.geom, ...) {
  # list of points can be treated as points
  .geom <- sf::st_sfc(unlist(.geom, recursive = FALSE, use.names = FALSE))
  st_makeline(.geom, ...)
}

st_makeline.numeric <- function(.x, .y, ...) {
  if (missing(.y)) {
    stop("You must provide at least a second coordinate `.y`.", call. = FALSE)
  }
  if (!is.numeric(.y)) {
    stop("`.y` must be numeric too", call. = FALSE)
  }
  if (length(.x) != length(.y)) {
    if(length(.x) != 1 && length(.y != 1)) {
      stop("`.x` and `.y` must be of the same length or length 1 to be recycled.", call. = FALSE)
    }
  }
}

cast_union <- function(.x, .y, .cast, .by_feature = FALSE, ...) {
  sf::st_cast(st_union(.x, .y, .by_feature), to = .cast, ...)
}

cast_combine <- function(.x, .cast, ...) {
  sf::st_cast(sf::st_combine(.x), to = .cast, ...)
}
}
