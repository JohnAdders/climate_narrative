#' Called on package attach
.onAttach <- function(libname, pkgname) {
  addResourcePath(
    "climate_narrative",
    system.file("www", package = "climate.narrative")
  )
}
