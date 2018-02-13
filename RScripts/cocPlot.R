#' cocPlot: Components of Change Chart, this is a county-level chart, regardless of output level
#'
#' @param  fips numeric, county-level FIPS code  ** will need to extract county level FIPS code in all calls
#' @param  ctyname County Name string, from input$unit
#' @param  lYr the last year of the output date range
#' @return ggplot2 graphic and data file
#' @export

cocPlot <- function(fips, ctyname,lYr,base=12) {

  yrLst <- seq(1990,lYr,1)

  f.coccty <- county_profile(as.numeric(fips), 1990:lYr, vars="totalpopulation,births,deaths,netmigration")%>%
    mutate( totalpopulation = as.numeric(totalpopulation),
            births=as.numeric(births),
            deaths=as.numeric(deaths),
            netmigration=as.numeric(netmigration),
            naturalIncrease=births-deaths)


  f.cocLong <- gather(f.coccty, TypeChange, Pop, c(births,deaths,netmigration))
  f.cocLong$TypeChange <- ifelse(f.cocLong$TypeChange =="netmigration","Net Migration",
                                 ifelse(f.cocLong$TypeChange =="births", "Births","Deaths"))

  f.cocLong$TypeChange <- factor(f.cocLong$TypeChange,
                                 levels=c("Births","Deaths", "Net Migration"))

  pltTitle <- "Components of Change:\nBirths, Deaths, and Net Migration"
  subTitle <- ctyname


  cocPlt <-  ggplot(data=f.cocLong,aes(x=year, y=Pop, colour=TypeChange)) +
    geom_line() +
    geom_point(aes(x=year, y=Pop, colour=TypeChange, shape=TypeChange),size=2) +
    scale_colour_manual("Type of Change", values=c("#82BC00", "#009ADD", "#5C666F")) +
    scale_shape_manual("Type of Change", values=seq(15, 17, 1)) +
    scale_x_continuous(breaks=seq(1990, lYr, 5)) +
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Population Change") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")


  f.coccty <- f.coccty[,2:8]
  names(f.coccty)  <- c("Place","Year","Total Population","Births","Deaths", "Net Migration","Natural Increase")


  outList <- list("plot" = cocPlt, "data" = f.coccty)

  return(outList)
}
