#' Create a geometry representing the bounding box of the geometry
#'
#' @param geom an `sfc` geometry.
#' @seealso `sf::st_bbox()`
#' @details This function does not aggregate by default (unlike `sf::st_bbox`), you will need `sf::st_combine()` to aggregate first.
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' x <- tibble(
#'        line = c(
#'          st_makeline(1:10, 11:20),
#'          st_makeline(1:10+2, 11:20 -2)
#'        ) %>%
#'        sf::st_set_crs(4326)
#' )
#'
#' mutate(x,
#'   envelope = st_envelope(line)
#' )
#' # aggregate
#' summarise(x,
#'   envelope = line %>% sf::st_combine() %>% st_envelope()
#' )
st_envelope <- function(geom) {
  res <- map(geom, envelope)
  res <- purrr::reduce(res, c)
  sf::st_set_crs(res, sf::st_crs(geom))
}

envelope <- function(x) {
  sf::st_bbox(x) %>%
    sf::st_as_sfc()
}
