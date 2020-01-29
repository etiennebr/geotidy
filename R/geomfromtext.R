#' Geometry from text
#'
#' Construct a geometry from OGC Well-Known text (WKT) description.
#' @param x WKT description
#' @param crs coordinate reference system
#'
#' @details It can be used as an equivalent to type casting in postgis (`'POINT(1 1)'::geometry`)
#' @return A geometry (class `sfc`)
#' @export
#'
#' @importFrom assertthat assert_that
#' @examples
#' st_geomfromtext('POLYGON((38 16,38 50,65 50,66 16,38 16))')
#' st_geomfromtext('POINT(-71.064544 42.28787)', crs = 4326)
#' # EWKT works as well
#' st_geomfromtext('SRID=4326;POINT(-71.064544 42.28787)')
#
# todo: move to sf
# todo: the function should be translated to type casting for postgis
#       because it is much faster than st_geomfromtext (~50 times).
st_geomfromtext <- function(x, crs = sf::NA_crs_) {
  assertthat::assert_that(is.character(x))
  sf::st_as_sfc(gsub("[\n\r]", " ", x), crs = crs)
}
