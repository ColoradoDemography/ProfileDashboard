#' tabList returns the list items from the outputList  based on the value of input$outChk
#'
#' @param item the item name in input$outChk
#' @return the content of each panel, drawn ffrom the output lists defined in the
#'      GLOBAL section of the  UI code
#' @export

tabList <- function(item){

  outList <- list("Section Not Defined")
  if(item == "stats") {
    outList <- stats.list
  }
  if(item == "popf") {
    outList <- popf.list
  }
  if(item == "pop") {
    outList <- popa.list
  }
  if(item == "popc") {
    outList <- popc.list
  }
  if(item == "housing") {
    outList <- poph.list
  }
  if(item == "comm") {
    outList <- popt.list
  }
  if(item == "emplind") {
    outList <- popei.list
  }
  if(item == "emply") {
    outList <- popem.list
  }
  return(outList)
}

