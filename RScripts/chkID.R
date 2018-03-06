#' chkID Returns a set of ids
#'
#' @param lvl the data level from input$level
#' @param fipslist the list of fips codes 
#' @param plName the placeName variable
#' @param ctyList the list of county names and information
#' @param plList the list of place names and information
#' @return a list of 6 items: ctyNum, ctyName, plNum, plName, multiCty and PlFilter
#' @export
#'
chkID <- function(lvl,fipslist,plName,ctyList,plList) {
  multiCty <- "F"
  PlFilter <- "T"
  
  if(lvl == "Municipalities/Places") { #the id is the place
    if(length(fipslist) > 1) {  #setting ctyNum to largest portion for a multi-county city
      plNum  <- substr(unique(fipslist),3,7)
      plName <-  unique(plList[which(plList$placefips == as.numeric(plNum)),3])
      pCty   <- plList[which(plList$placefips == as.numeric(plNum)),]
      ctyNum <- pCty[which(pCty$cty_Pop == max(pCty$cty_Pop)),1]
      ctyNum <- str_pad(ctyNum,3,pad="0")
      ctyName <- ctyList[which(ctyList$countyfips == as.numeric(ctyNum)),3]
      multiCty <- "T"   
    } else {
      plNum   <- substr(fipslist,3,7)
      plName  <-  plList[which(plList$placefips == as.numeric(plNum)),3]
      ctyNum  <- plList[which(plList$placefips == as.numeric(plNum)),1]
      ctyNum  <- str_pad(ctyNum,3,pad="0")
      ctyName <- ctyList[which(ctyList$countyfips == as.numeric(ctyNum)),3]
    }
    PopCheck <- as.numeric(plList[which(plList$placefips == as.numeric(plNum)),5])
    PopCheck <- as.numeric(unique(PopCheck))
    if(PopCheck > 200){
      PlFilter= "F"
    } 
  }    

  if(lvl == "Counties") {
    ctyName <- plName
    ctyNum <- substr(fipslist,3,5)
    plNum <- ""
    plName <- ""
  }
  outList <- list("ctyNum" = ctyNum, "ctyName" = ctyName, "plNum" = plNum, "plName" = plName, "multiCty" = multiCty,"PlFilter" = PlFilter)
  return(outList)
}