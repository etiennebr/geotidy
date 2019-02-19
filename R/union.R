#' Union geometries
#'
#' Union geometries to create MULTI* geometries in the case of different
#' geometries. If geometries are identical, the union returns a single geometry.
#' If you need to preserve all geometries, use [sf::st_combine()]. This function
#' behaves differently than `sf` because it strictly returns 1 or `length(x)`
#' geometries.
#'
#' @param x A geometry or a set of geometries of class `sfc` to be unioned.
#' @param y (optional). A geometry or a set of geometries of class `sfc`.
#' @param by_feature default is `TRUE`. Union each element of `x` with
#'   corresponding `y` element. If `FALSE`, all the features are first unioned
#'   row-wise, then all unioned together.
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
#' summarise(geom, union = st_union(origin, destin, by_feature = FALSE))
#' @export
#' @seealso [sf::st_union()], [sf::st_combine()]
st_union <- function(x, y, by_feature = TRUE) {
  if (!missing(y)) {
    if (length(x) != length(y)) {
      stop("`x` and `y` should be of the same length.",
           "`length(x)` is ", length(x),
           " and `length(y)` is ", length(y), ".",
           call. = FALSE)
    }
    if (!by_feature) {
      # When ,providing two vectors with `by_feature = TRUE`, the vectors are
      # first unioned by row and then together.
      uni <- st_union(x, y, by_feature = TRUE)
      return(st_union(uni, by_feature = FALSE))
    }
    return(map2_sfc(x, y, ~sf::st_union(.x, .y, by_feature = by_feature)))
  }
  sf::st_sfc(sf::st_union(x, by_feature = by_feature))
}
