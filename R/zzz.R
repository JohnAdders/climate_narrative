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
  # additionally, explicitly define resource paths for some libraries
  # (for some reason does not work automatically on CGFI server)
  folder <- system.file("htmlwidgets/lib", package="tippy")
  addResourcePath(dir(folder), paste0(folder, "/", dir(folder)))
  addResourcePath(paste0("tippy-binding-", "0.0.1"), system.file("www", package="htmlwidgets"))
  addResourcePath(paste0("htmlwidgets-", packageVersion("htmlwidgets")), system.file("htmlwidgets", package="tippy"))
  addResourcePath(paste0("mdInput-", packageVersion("shinymarkdown")), system.file("assets", package="shinymarkdown"))
  options(stringsAsFactors = FALSE)
}
