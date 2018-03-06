#' setAxis Sets the range of the quanitiative axis (mostly the Y-axis)
#'
#' Copied from "ms_popage" in codemgprofile, Modified by AB 12/2017
#' Creates a Chart comparing Forecast Population Growth by Age in Colorado.
#'
#' @param inValue is the quantitative scale to be input, typically a quantitive field from a data frame
#' @return A list consisting of3 values:  "minBrk", "maxBrk" to be used in the "limits=" statement
#'         and "yBrk" to be used in the "breaks=" statement of the scale_y_continuous command in the ggplot call
#' @export

setAxis <- function(inValue) {
  maxval <- ceiling(max(inValue)) 
  minval <- floor(min(inValue))
  yBrk <- pretty(minval:maxval,n=5)
  minBrk <- min(yBrk)
  maxBrk <- max(yBrk)
  
  if(maxBrk  == maxval){
    addval <- maxBrk + 5
    maxBrk <- addval
    yBrk <- append(yBrk,addval)
  }
  outList <- list("minBrk" = minBrk, "maxBrk" = maxBrk, "yBrk" = yBrk)
 return(outList)
}
