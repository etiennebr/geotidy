#' Count the number of points in a geometry
#'
#' @param .geom A geometry object.
#' @family make
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' tibble(x = 1:2, y = 3:4) %>%
#'   mutate(geom = st_makepoint(x, y), n = st_npoints(geom)) %>%
#'   summarise(line = st_makeline(geom), n = st_numpoints(line))
st_numpoints <- function(.geom) UseMethod("st_numpoints")

#' @export
st_numpoints.sfc_POINT <- function(.geom) {
  rep_len(1, length.out = length(.geom))
}

#' @export
st_numpoints.sfc_MULTIPOINT <- function(.geom) {
  purrr::map_int(.geom, nrow)
}

#' @export
st_numpoints.sfc_LINESTRING <- function(.geom) {
  purrr::map_int(.geom, nrow)
}

#' @rdname st_numpoints
#' @export
st_npoints <- st_numpoints
