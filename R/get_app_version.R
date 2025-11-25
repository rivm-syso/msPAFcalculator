#' Gets the version number of the app based on the Description file
#'
#' @returns
#' @export
#'
#' @examples
getAppVersion <- function() {
  if (file.exists("VERSION")) {
    readLines("VERSION", warn = FALSE)[1]
  } else if (file.exists("DESCRIPTION")) {
    desc <- read.dcf("DESCRIPTION")
    desc[1, "Version"]
  } else {
    "unknown"
  }
}