#' popForecast Creates a Chart showing population and estmates
#'
#'
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param byr is the first year of the series to be extracted by county_sya (min 2000)
#' @param eyr is the last  year of the series to be extracted by county_sya (max 2050)
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

popForecast <- function(fips, ctyname, byr=2000,eyr=2050, base=10) {

  fips=as.numeric(fips)
  yrs <- seq(byr,eyr,2)
  d <- county_sya(fips, yrs) %>%
    group_by(county, datatype, year) %>%
    summarize(Tot_pop = sum(as.numeric(totalpopulation)))
  

  yaxs <- setAxis(d$Tot_pop)
  xaxs <- setAxis(d$year)


  p=d%>%
    ggplot(aes(x=year, y=round(Tot_pop, digits=0), group=datatype))+
    geom_line(aes(linetype=datatype), color="#00953A", size=1.75) +
    labs(x="Year", y="Population", title=paste("Population Forecast,", byr, "to", eyr, sep=" "),
         subtitle = ctyname,
         caption = captionSrc("SDO",""))+
    scale_y_continuous(limits=c(yaxs$minBrk,yaxs$maxBrk), breaks=yaxs$yBrk, label=comma)+
    scale_x_continuous(limits=c(xaxs$minBrk,xaxs$maxBrk),breaks= xaxs$yBrk) +
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x=element_text(angle=90,size=12),
          axis.text.y = element_text(size=12),
          legend.title=element_blank())

  # Creating Output data file
  d[4] <- round(d[4],digits=0)
  d$county <- ctyname
  outList <- list("plot" = p,"data" = d)
  return(outList)
}
