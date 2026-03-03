#' Function to get the home directory as a function of the platform
#' @export
#' @examples
#' get_home()
get_home <- function() {
  if (.Platform$OS.type == "windows") {
    Sys.getenv("USERPROFILE")
  } else {
    Sys.getenv("HOME")
  }
}