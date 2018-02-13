#' downloadObjUI  File Download Modules
#'
#' downloadObjUI is the UI function that creates the download buttons
#'
#' @param id is the data name and creates the module/namespace ID
#' @return output button type (plot or data)
#' @export

downloadObjUI <- function(id) {

  ns <- NS(id)
  #Identifying data object and type
  if(nchar(id) == 9) {
       dtype <- substr(id,6,9)
  }
  if(nchar(id) == 10) {
    dtype <- substr(id,7,10)
  }


  #setting button label
  outLabel <- ifelse(dtype== "plot","Download Plot","Download Data")

  downloadButton(ns("download"),outLabel)
}
