#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

is_geometry <- function(x) {
  inherits(x, "sfc")
}
