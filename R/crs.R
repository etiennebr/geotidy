#' Spatial Reference System
#' @inheritParams sf::st_set_crs
#' @export
#' @rdname crs
st_set_crs <- sf::st_set_crs


#' @details `st_setsrid` is postgis compatible
#' @inheritParams sf::st_set_crs
#' @export
#' @rdname crs
st_setsrid <- st_set_crs

unique_crs <- function(x) {
  stopifnot(is.list(x))
  crs <- unique(map_crs(x))
  if (length(crs) > 1) {
    stop("Geometries must be of the same crs. Found ", length(crs), " crs: ", paste(crs, collapse = ", "), call. = FALSE)
  }
  crs[[1]]
}

map_crs <- function(x) {
  out <- purrr::map(x, purrr::safely(sf::st_crs))
  out <- purrr::transpose(out)
  if (!all(purrr::map_lgl(out$error, is.null)) || any(purrr::map_lgl(out$result, ~!is_crs(.x)))) {
    i <- which(purrr::map_lgl(out$result, ~!is_crs(.x)))
    stop("Can't coerce element ", i[1], " from a ", class(x[[i[1]]])[1], " to a crs.", call. = FALSE)
  }
  out$result
}
