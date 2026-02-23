#' Function to install, if not already installed, and load a package
#' @export
#' @examples
#' myloadpackage(c('maps','ggplot2'))

myloadpackage <- function(x){
  sapply(x,function(k) {if (!k %in% installed.packages()) install.packages(k)}) 
  lapply(x, require, character.only=T)

  }

