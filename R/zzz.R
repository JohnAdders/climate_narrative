#' Called on package attach
#'
#' It turned out that it is necessary to define the resource path at this point, otherwise
#' it was not possible to deploy the app properly. John Adcock claims this is a well known issue
#' and a typical solution
#'
#' @param libname Not used
#' @param pkgname Not used
#' @importFrom utils packageVersion
#' @importFrom yaml read_yaml
#'
.onAttach <- function(libname, pkgname) {
  addResourcePath(
    "climate_narrative",
    system.file("www", package = "climate.narrative")
  )
  # additionally, explicitly define resource paths for some libraries
  # (for some reason does not work automatically on CGFI server)
  addResourcePath(paste0("tippy-binding-", utils::packageVersion("tippy")), system.file("htmlwidgets", package = "tippy"))
  addResourcePath(paste0("htmlwidgets-", utils::packageVersion("htmlwidgets")), system.file("www", package = "htmlwidgets"))
  addResourcePath(paste0("mdInput-", utils::packageVersion("shinymarkdown")), system.file("assets", package = "shinymarkdown"))
  tippy_dependencies <- yaml::read_yaml(system.file("htmlwidgets/tippy.yaml", package = "tippy"))
  dep <- tippy_dependencies[[1]][[1]]
  addResourcePath(paste0(dep$name, "-", dep$version), system.file(dep$src, package = "tippy"))
  options(stringsAsFactors = FALSE)
}
