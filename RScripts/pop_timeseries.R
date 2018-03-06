#' pop_timeseries Creates a \code{ggplot2} chart of the population for a CO county
#'
#' This is a replacement for county_ts_chart  Copied from codemogProfile AB 3/2018
#'
#' Takes some basic input on the time period and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 2000 to 2040 (beyond 2013 are
#' forecasts).
#'
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The last year in the timeseries Defaults to 2013.
#' @param base Base font size.
#' @return ggplot2 graphic and data file
#' @export



pop_timeseries=function(fips, beginyear=2000,endyear, base=10){

  # Checking length of fips to idenify municipality data series
  if(nchar(fips) == 5) {
    fipsn <- as.numeric(fips)
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE (placefips = ",fipsn,") and (year >= ",beginyear,") and (year <= ",endyear,");")
    # Postgres Call to gather municipal jobs numbers
    pw <- {
      "demography"
    }
    
    # loads the PostgreSQL driver
    drv <- dbDriver("PostgreSQL")
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- dbConnect(drv, dbname = "dola",
                     host = "104.197.26.248", port = 5433,
                     user = "codemog", password = pw)
    rm(pw) # removes the password
    
    d1 <-  dbGetQuery(con, sqlStrPop1)
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)

    d1 <- d1[which(d1$countyfips != 999), ]  # removing "Total" for multi-county cities
    d <- d1 %>% group_by(placefips, municipalityname, year) %>% summarize(totalPopulation = sum(totalpopulation))
    d$placename <-sub(' \\([P,p]art\\)','',d$municipalityname)
    
   } else { #fips is a county code
        fipsn=as.numeric(fips)
      
        d=county_profile(fipsn, beginyear:endyear, "totalpopulation")%>%
          select(countyfips, county, year, totalPopulation=totalpopulation)
        d$placename <- paste0(d$county, " County")
   }
        
  d$totalPopulation <- as.numeric(d$totalPopulation)

  yaxs <- setAxis(d$totalPopulation)
  xaxs <- setAxis(d$year)
  if(xaxs$maxBrk != endyear) {
    xaxs$maxBrk <- endyear
    xaxs$yBrk[length(xaxs$yBrk)] <- endyear
  }


  p=d%>%
    ggplot(aes(x=year, y=totalPopulation))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Population", title=paste("Population,", beginyear, "to", max(d$year), sep=" "),
         subtitle = d$placename,
         caption = captionSrc("SDO",""))+
    scale_y_continuous(limits=c(yaxs$minBrk,yaxs$maxBrk), breaks=yaxs$yBrk, label=comma)+
    scale_x_continuous(limits=c(xaxs$minBrk,xaxs$maxBrk), breaks=xaxs$yBrk) +
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x=element_text(angle=90,size=12),
          axis.text.y = element_text(size=12))

  # Bind List
  outList <- list("plot" = p, "data" = d)

  return(outList)
}

