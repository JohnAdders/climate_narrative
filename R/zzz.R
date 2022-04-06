#' Called on package attach
#'
#' It turned out that it is necessary to define the resource path at this point, otherwise
#' it was not possible to deploy the app properly. John Adcock claims this is a well known issue
#' and a typical solution
#'
#' @param libname Not used
#' @param pkgname Not used
#'
.onAttach <- function(libname, pkgname) {
  addResourcePath(
    "climate_narrative",
    system.file("www", package = "climate.narrative")
  )
  addResourcePath(
    "lib",
    system.file("www/lib", package = "climate.narrative")
  )
  options(stringsAsFactors = FALSE)
}
