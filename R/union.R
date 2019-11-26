#' Union geometries
#'
#' Union geometries to create MULTI* geometries in the case of different
#' geometries. If geometries are identical, the union returns a single (MULTI)
#' geometry.
#' If you need to preserve all geometries, use [sf::st_combine()].
#'
#' This function mimics postgis behavior, which is differently than `sf` because
#' it strictly returns 1 or `length(x)` geometries.
#'
#' @param x A geometry or a set of geometries of class `sfc` to be unioned.
#' @param y An optional geometry or a set of geometries of class `sfc`.
#'
#' @details When provided two elements, each element of `x` is unioned with
#'   corresponding `y` element. When provided only `x`, then all the rows
#'   are unioned together to provide a single geometry.
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' geom <- tibble(
#'   x = 1:3,
#'   y = 4:6,
#'   origin = st_point(x, y),
#'   destin = st_point(x + 1, y + 1)
#' )
#' geom <- mutate(geom, union = st_union(origin, destin))
#' geom
#' summarise(geom, union = st_union(origin, destin))
#' @export
#' @seealso [sf::st_union()], [sf::st_combine()]
st_union <- function(x, y) {
  if (!missing(y)) {
    return(map2_sfc(x, y, ~sf::st_union(.x, .y, by_feature = TRUE)))
  }
  sf::st_sfc(sf::st_union(x, by_feature = FALSE))
}
