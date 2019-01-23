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
#' tibble::tibble(g = c("a", "a"), point = c(st_point(12, 21), st_point(21, 12)))
#'   group_by(g) %>%
#'   summarise(multi = st_multi(point))
#' @export
#' @family make
st_multi <- function(.geom, ...) UseMethod("st_multi")

st_multi.sfc_POINT <- function(.geom, ...) {
  sf::st_cast(sf::st_union(.geom), to = "MULTIPOINT")
}

st_multi.sfc_MULTIPOINT <- function(.geom, ...) {
  sf::st_union(.geom)
}
