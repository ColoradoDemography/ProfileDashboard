#' county_timeseries Creates a \code{ggplot2} chart of the population for a CO county
#'
#' This is a replacement for county_ts_chart  Copied from codemogProfile
#'
#' Takes some basic input on the time period and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 1990 to 2040 (beyond 2013 are
#' forecasts).
#'
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endYear The last year in the timeseries Defaults to 2013.
#' @param base Base font size.
#' @return ggplot2 graphic and data file
#' @export



county_timeseries=function(fips, beginyear=1990,endYear, base=10){

  fips=as.numeric(fips)

  d=county_profile(fips, beginyear:endYear, "totalpopulation")%>%
    select(countyfips, county, year, totalPopulation=totalpopulation)
  d$county <- paste0(d$county, " County")

  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Population", title=paste("Population,", beginyear, "to", max(d$year), sep=" "),
         subtitle = d$county,
         caption = captionSrc("SDO",""))+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x=element_text(angle=90,size=8))

  # Bind List
  outList <- list("plot" = p, "data" = d)

  return(outList)
}

