#' captionSrc formats the captions for the tables and plots
#'
#' @param type the data type, "SDO" for State Demography Office data, "ACS" for American Community Survey data
#' @param dataSrc The data string for ACS data
#' @return  Date-stamped caption string
#' @export
#'
captionSrc <- function(type, dataSrc) {

  dateStr <- paste0("Print Date: ",as.character(format(Sys.Date(),"%m/%d/%Y")))

  if(type == "SDO") {
    srcStr <- paste0("Source: State Demography Office ", dateStr)
  }

  if(type == "SDOBEA") {
    srcStr <- paste0("Source: State Demography Office and U.S. Bureau of Economic Analysis ", dateStr)
  }
  if(type == "LODES") {
    srcStr <- paste0("Source: U.S. Census Bureau LEHD Origin-Destination Employment Statistics (LODES) ", dateStr)
  }
  if(type == "QCEW") {
    srcStr <- paste0("Source: Department of Labor and Employment (QCEW) ", dateStr)
  }
  if(type =="ACS") {
    byr <- paste0("20",substr(dataSrc,4,5))
    eyr <- paste0("20",substr(dataSrc,6,7))
    srcStr <- paste0("Source: U.S. Census Bureau, ",byr,"-",eyr," American Community Survey. ", dateStr)
  }
  return(srcStr)
}
