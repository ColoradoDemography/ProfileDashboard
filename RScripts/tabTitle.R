#' tabTitle manages the output of descriptive tabs in the interface
#'
#' @param item the item name in input$outChk
#' @return  Descriptive string provided in the tabs of the main interface
#' @export
#'
tabTitle <-function(item) {
  outTitle <- switch(item,
                     "stats" = "Basic Statistics",
                     "popf" = "Population Trends",
                     "pop" ="Age",
                     "popc"= "Income, Race and Education",
                     "housing" = "Housing and Households",
                     "comm" = "Commuting and Job Growth",
                     "emplind" = "Employment by Industry",
                     "emply" = "Employment Forecast and Wage Information")
  return(outTitle)
}
