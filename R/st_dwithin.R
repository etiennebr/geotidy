#' Are two geometries within a specified distance
#'
#' Compare two geometries to test if they are within a given distance. Both
#' geometries must use the same coordinate system.
#' @param x geometry of class `sfc`.
#' @param y geometry of class `sfc`.`
#' @param distance Numeric. Maximal distance threshold in map units.
#'
#' @return Returns `TRUE` for each `x` when `y` is within the specified distance.
#' `y` is recycled only if it is of length 1.
#' @details Using `st_dwithin` computes the exact pairwise distance and
#'   leverages the spatial index if available, which is preferable to using
#'   `st_buffer()`, which are an approximation. Since distance are computed
#'   pair-wise, it is generally much faster since it does not require to create
#'   a geometry. It should also be faster than using `st_distance(x, y) <
#'   distance`.
#'
#'   **Postgis** does a cartesian join on the two geometry vectors; complying
#'   would break `geotidy` rule of not recycling vectors of length > 1. It is
#'   still possible to mimic the behavior by joining the data before with
#'   `dplyr::full_join`, but you have to do it explicitly.
#' @export
#'
#' @importFrom sf st_sfc
#' @importFrom purrr map2_lgl
#' @seealso sf::st_buffer(), sf::st_distance()
#' @examples
#' library(geotidy)
#' library(dplyr)
#' # points against line
#' line <- st_makeline(st_point(1:10, rnorm(10)))
#' plot(line)
#' x <- tibble::tibble(geometry = st_point(1:10, 1)) %>%
#'   mutate(within_1 = st_dwithin(geometry, line, 1))
#' plot(x$geometry, add = TRUE, pch = ifelse(x$within_1, 16, 1))
#'
#' # points against points
#' x <- tibble::tibble(
#'   school = st_point(1:10, rnorm(10)),
#'   house = st_point(runif(10) * 10, 1:10)
#' ) %>%
#'   mutate(within_3 = st_dwithin(school, house, 3))
#' plot(sf::st_union(x$school, x$house))
#' plot(x$school, add = TRUE)
#' plot(x$house, pch = 3, add = TRUE)
#' plot(x$school, add = TRUE, pch = ifelse(x$within_3, 16, 1))
st_dwithin <- function(x, y, distance) {
  if (missing(y)) stop("`st_dwithin` requires two geometry columns. Column `y` is missing.")
  if (length(y) == 1) y <- sf::st_sfc(rep_len(y, length(x)))
  if (length(x) != length(y)) {
    stop("`x` and `y` must be of same length, or `y` can be of length 1 (it is recycled).")
  }

  dist_mat <- sf::st_is_within_distance(x, y, distance, TRUE)
  purrr::map2_lgl(dist_mat, seq_along(x), ~.y %in% .x)
}
