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
  addResourcePath(paste0("mdInput-", utils::packageVersion("shinymarkdown")), system.file("assets", package = "shinymarkdown"))
  # this option is by default different in most recent version of R (FALSE) than older ones (TRUE)
  # setting it explicitly prevents some issues
  options(stringsAsFactors = FALSE)
}
