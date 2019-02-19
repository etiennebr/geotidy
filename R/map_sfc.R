#' Apply a function to each element of a vector and bind to an sfc object
#'
#' @rdname map_sfc
#' @inheritParams purrr::map2
#' @param .crs Coordinate Reference System. See [sf::st_crs()] for details.
#' @param .precision Numeric. Precision applied to every coordinate, expressed
#'   as a scale factor. For instance 1000 to round to three decimals.
#' @param .check_ring_dir Logical. Check that the polygon ring directions are
#'   counter clockwise and holes are clockwise, order is correct it if not.
#'   Default to `FALSE``.
#' @seealso [purrr::map()] [sf::st_sfc]
map_sfc <- function(.x, .f, ..., .crs = NA_crs_, .precision = 0, .check_ring_dir = FALSE) {
  o <- purrr::map(.x, .f, ...)
  sf::st_sfc(o, crs = .crs, precision = .precision, check_ring_dir = .check_ring_dir)
}

#' @rdname map_sfc
map2_sfc <- function(.x, .y, .f, ..., .crs = NA_crs_, .precision = 0, .check_ring_dir = FALSE) {
  o <- purrr::map2(.x, .y, .f, ...)
  sf::st_sfc(o, crs = .crs, precision = .precision, check_ring_dir = .check_ring_dir)
}
