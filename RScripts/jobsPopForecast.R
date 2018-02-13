#' jobsPopForecast Produces a plot and table showing changes in forecasted jobs and population
#'  for the period from 2010 to 2035
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param fips is the fips code for the county being examined
#' @param ctyname the name of the place being output
#'
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or latex table and a dataset
#' @export

jobsPopForecast <- function(fips, ctyname, base=10){
  #fips is the 3-digit character string

  # creating alternative fips code for Denver MSA
  if(fips %in% c("001", "005", "013", "014", "031", "035", "059")) {
    fips = "500"
    MSAList <- c(1,5,13,14,31,35,59)
    ctyname = "Denver-Boulder MSA"
  } else {
    MSAList <- as.numeric(fips)
  }

  jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = '",as.numeric(fips), "';")


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

  # Read data files

  f.totalJobs <- dbGetQuery(con, jobsSQL)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  f.totalJobs$type <- "Jobs Estimate"

  # Gathering population data
  f.Pop =county_sya(MSAList, 1990:2040,"totalpopulation")
  f.Pop$totalpopulation <- as.numeric(f.Pop$totalpopulation)
  f.totalPop <- f.Pop %>%
    group_by(year, datatype) %>%
    summarize(TotalPop = sum(totalpopulation))
  f.totalPop$type <- "Population Estimate"


  x <- as.data.frame(f.totalJobs[,c(3,5,4)])
  names(x) <- c("year","type","people")

  y <- as.data.frame(f.totalPop[,c(1,4,3)])
  names(y) <- c("year","type","people")

  f.plotdata <- rbind(x,y)
  f.plotdata <- f.plotdata[which(f.plotdata$year >= 2010 & f.plotdata$year <= 2040),]
  f.plotdata$type <- factor(f.plotdata$type, c("Jobs Estimate","Population Estimate"))

  pltTitle <- paste0("Forecast Change in Jobs and Population\n",as.character(min(f.plotdata$year))," to ",as.character(max(f.plotdata$year)))

  Plot <- f.plotdata %>%
    ggplot(aes(x=year, y=people, color=type))+
    geom_line(size= 1.5)+
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    scale_y_continuous(label=comma)+
    scale_x_continuous(breaks=seq(2010,2035,5),expand = c(0, 0)) +
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Change") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")


  # producing the table...
  f.forecastdata <- merge(x,y,by="year")
  f.forecastdata$geoname <- ctyname
  f.forecastdata <- f.forecastdata[,c(1,6,3,5)]
  f.forecastdata <- f.forecastdata[which(f.forecastdata$year >= 2010 & f.forecastdata$year <= 2040),]
  names(f.forecastdata) <- c("Year","Place","Jobs","Population")
  f.forecastdata$Jobs <- format(f.forecastdata$Jobs,big.mark=",")
  f.forecastdata$Population <- format(f.forecastdata$Population,big.mark=",")


  outList <- list("plot" = Plot, "data" = f.forecastdata)


  return(outList)
}
