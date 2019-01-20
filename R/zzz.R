# setup_repo
.geotidy_setup <- function() {
  usethis::use_readme_rmd()
  usethis::use_testthat()
  usethis::use_roxygen_md()
  usethis::use_mit_license()
  usethis::use_git()
  usethis::use_github()
  usethis::use_travis()
  usethis::use_coverage()
  usethis::use_lifecycle_badge("experimental")
  usethis::use_pkgdown()
  usethis::use_pkgdown_travis()
  usethis::use_github_labels(
    labels = .geotidy_labels(),
    colours = .geotidy_labels_brewer(),
    descriptions = .geotidy_labels_descriptions(),
    delete_default = TRUE
  )
  # https://ropenscilabs.github.io/travis/
  # travis::travis_enable()
  # travis::use_travis_deploy()
}

.geotidy_replacements <- function() {
  c(documentation = "documentation :bulb:", bug = "bug :bug:")
}

.geotidy_labels_brewer <- function(labels = .geotidy_labels(), palette = "Pastel1", order = seq_along(labels)) {
  colors <- RColorBrewer::brewer.pal(n = length(labels), name = palette)[order]
  purrr::set_names(sub("#", "", colors), labels)
}

.geotidy_labels <- function(replacements = .geotidy_replacements()) {
  .replace(usethis::tidy_labels(), replacements)
}

.geotidy_labels_descriptions <- function(replacements = .geotidy_replacements()) {
  .rename(usethis::tidy_label_descriptions(), replacements)
}

.rename <- function(original, replacements) {
  i <- match(names(original), names(replacements))
  names(original)[!is.na(i)] <- purrr::set_names(replacements[stats::na.omit(i)], NULL)
  original
}

.replace <- function(original, replacements) {
  i <- match(original, names(replacements))
  original[!is.na(i)] <- purrr::set_names(replacements[stats::na.omit(i)], NULL)
  original
}
