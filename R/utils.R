#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

is_geometry <- function(x) {
  inherits(x, "sfc")
}

is_crs <- function(x) {
  inherits(x, "crs")
}

is_geometry_col <- function(x) {
  vapply(x, is_geometry, logical(0))
}
