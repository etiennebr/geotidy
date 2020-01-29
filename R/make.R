# all direct exports of sfc methods from sf
#
# sfc <- sloop::s3_methods_class("sfc")
# sfc %>%
# mutate(generic = glue::glue())

#' Create points
#'
#' Create points from coordinates. `st_point` is simply a wrapper around `st_makepoint`.
#'
#' `x` amd `y` must be of the same length. If one of the coordiantes are
#' missing, it creates and empty point geometry.
#'
#' @return List column of points with the same length than `x` and `y`.
#'
#' @rdname st_point
#' @param x Numeric vector of x coordinates (e.g. longitude) or a list of pairs
#'   of coordinates. In the latter case `y` can be missing.
#' @param y Numeric vector of y coordinates (e.g. latitude).
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
st_makepoint <- function(x, y) {
  if (base::missing(y)) {
    return(map_sfc(x, sf::st_point))
  }
  map2_sfc(x, y, ~sf::st_point(c(.x, .y)))
}

#' @rdname st_point
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
  cast_combine(.geom, .cast = "MULTIPOINT")
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
#' @family make
#' @export
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' x <- tibble(
#'   g = c("a", "a", "b"),
#'   point = c(st_point(12, 21), st_point(21, 12), st_point(11, 11))
#' )
#' x %>%
#'   group_by(g) %>%
#'   # make sure we create lines with more than one point
#'   filter(n() > 1) %>%
#'  summarise(line = st_makeline(point))
#' @export
st_makeline <- function(.geom, .to, ...) UseMethod("st_makeline")

#' @export
st_makeline.sfc_POINT <- function(.geom, .to, ...) {
  if(!missing(.to)) {
    return(cast_combine(.geom, .to, .cast = "LINESTRING", .by_feature = TRUE))
  }
  if (sum(st_numpoints(.geom)) < 2) {
    # We could return:
    #   - a linestring with a single vertex (postgis does it)
    #   - an empty linestring
    #   - the point
    # But we go for a strict behavior
    stop("A line must contain at least 2 points.", call. = FALSE)
  }
  cast_combine(.geom, .cast = "LINESTRING")
}

#' @export
st_makeline.sfc_MULTIPOINT <- function(.geom, .to, ...) {
  # multipoints can be treated as points
  st_makeline.sfc_POINT(.geom)
}

#' @export
st_makeline.list <- function(.geom, .to, ...) {
  # list of points can be treated as points
  .geom <- sf::st_sfc(unlist(.geom, recursive = FALSE, use.names = FALSE))
  st_makeline(.geom)
}

#' @export
st_makeline.numeric <- function(.geom, .to, ...) {
  x <- .geom
  if (missing(.to)) {
    stop("You must provide at least a second coordinate `y`.", call. = FALSE)
  }
  y <- .to
  if (!is.numeric(y)) {
    stop("`y` must be numeric too", call. = FALSE)
  }
  if (length(x) != length(y)) {
    if (length(x) != 1 && length(y) != 1) {
      stop("`x` and `y` must be of the same length or length 1 to be recycled.", call. = FALSE)
    }
    if (length(x) == 1) x <- rep(x, length(y))
    if (length(y) == 1) y <- rep(y, length(x))
  }
  st_makeline(purrr::map2(x, y, st_point))
}

cast_union <- function(x, y, .cast, .by_feature = FALSE, ...) {
  sf::st_cast(st_union(x, y, .by_feature), to = .cast, ...)
}

cast_combine <- function(x, y, .cast, .by_feature = FALSE, ...) {
  if (!.by_feature) {
    return(sf::st_cast(sf::st_combine(x), to = .cast, ...))
  }
  if(missing(y)) stop("If `.by_feature` is TRUE, you must provide `y` to combine with `x`.", call. = FALSE)
  pairs <- map2_sfc(x, y, ~c(.x, .y), crs = unique_crs(x))
  sf::st_cast(pairs, to = .cast, ...)
}

#' Dump vertex to a nested tibble of points
#'
#' Creates a geometry column containing a tibble where each vertex is a row.
#' `st_dumppoints` is useful for expanding geometries. It is the reverse of a GROUP BY
#' in that it creates new rows. For example it can be used to expand
#' MULTIPOLYGONS into POLYGONS.
#' @param x Geometry `sfc` column
#' @rdname st_coordinates
#' @return A list of tibbles.
#' @seealso [sf::st_cast()], [st_coordinates()], [sf::st_coordinates()]
#' @export
st_dumppoints <- function(x) {
  purrr::map(x, .st_dumppoints)
}

#' @importFrom rlang .data
.st_dumppoints <- function(x) {
  dplyr::transmute(
    .st_coordinates(x),
    geom = st_point(.data$.x, .data$.y),
    path = row_number()
    )
}

#' List of vertex coordinates
#'
#' @rdname st_coordinates
#' @details `st_coordinates` returns a tibble containing coordinates, also some
#'   grouping features. `POINT`: `.path = 1`; `MULTIPOINT`: `.path` orders the
#'   points; `LINESTRING`: ``
#' @export
st_coordinates <- function(x) {
  purrr::map(x, .st_coordinates)
}

.st_coordinates <- function(.x) {
  tibble::as_tibble(sf::st_coordinates(.x)) %>%
    purrr::set_names(~tolower(paste0(".", .x))) %>%
    dplyr::mutate(.path = dplyr::row_number())
}

#' Dump vertex to a nested tibble of points
#'
#' Creates a geometry column containing a tibble where each vertex is a row.
#' `st_dumppoints` is useful for expanding geometries. It is the reverse of a GROUP BY
#' in that it creates new rows. For example it can be used to expand
#' MULTIPOLYGONS into POLYGONS.
#' @param x Geometry `sfc` column
#' @return A list of tibbles.
#' @seealso [sf::st_cast()], [st_coordinates()]
#' @export
st_dumppoints <- function(x) {
  purrr::map(x, .st_dumppoints)
}

.st_dumppoints <- function(x) {
  .y <- NULL
  .x <- NULL
  .st_coordinates(x) %>%
    transmute(geom = st_point(.x, .y), path = row_number())
}


st_coordinates <- function(x) {
  purrr::map(x, .st_coordinates)
}

.st_coordinates <- function(.x) {
  tibble::as_tibble(sf::st_coordinates(.x)) %>%
    purrr::set_names(~tolower(paste0(".", .x))) %>%
    dplyr::mutate(.path = dplyr::row_number())
}
