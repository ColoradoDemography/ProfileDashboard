#' weeklyWages Produces a plot and dataset showing  average weekly wages
#'  for the period from 2000 to the present
#'
#'
#' @param fips is the fips code for the county being examined
#' @param ctyname the name of the place being output
#'
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or lates table and a dataset
#' @export
#'

weeklyWages <- function(fips, ctyname, base=10){

  wagePLSQL <- paste0("SELECT * FROM estimates.weekly_wages WHERE fips = '",as.numeric(fips), "';")
  wageSTSQL <- paste0("SELECT * FROM estimates.weekly_wages WHERE fips = '0';")


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

  f.wagePL <- dbGetQuery(con, wagePLSQL)
  f.wageST <- dbGetQuery(con, wageSTSQL)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)


  # Creating long data set for Place data
  f.wagePL_L <- gather(f.wagePL,year,wages, wkwage_2001:wkwage_2016)
  f.wagePL_L$year <- as.numeric(gsub("wkwage_","",f.wagePL_L$year))
  f.wagePL_L$wages <- as.numeric(f.wagePL_L$wages)
  f.wagePL_L$fmt_wages <- paste0("$", formatC(as.numeric(f.wagePL_L$wages), format="f", digits=0, big.mark=","))
  f.wagePL_L <- f.wagePL_L[which(f.wagePL_L$year >= 2001),]
  f.wagePL_L$geoname <- ctyname

  # Creating long data set for State data
  f.wageST_L <- gather(f.wageST,year,wages, wkwage_2001:wkwage_2016)
  f.wageST_L$year <- as.numeric(gsub("wkwage_","",f.wageST_L$year))
  f.wageST_L$wages <- as.numeric(f.wageST_L$wages)
  f.wageST_L$fmt_wages <- paste0("$", formatC(as.numeric(f.wageST_L$wages), format="f", digits=0, big.mark=","))
  f.wageST_L <- f.wageST_L[which(f.wageST_L$year >= 2001),]
  f.wageST_L$geoname <- "Colorado"

  #Preparing the Plot

  f.plot <- rbind(f.wagePL_L, f.wageST_L)

  maxYr <- as.numeric(max(f.plot$year))
  f.plot <- f.plot[which(f.plot$year %in% seq(2001,maxYr,3)),]

  axs <- setAxis(f.plot$wages)
  #axs$maxBrk <- axs$maxBrk + (1500 - axs$maxBrk) 

  f.plot$geoname <- factor(f.plot$geoname,levels=c(ctyname,"Colorado"))

  pltTitle <- paste0("Average Weekly Wage, in Real (",max(f.plot$year),") Dollars")

  Plot <- f.plot %>%
    ggplot(aes(x=year, y=wages, colour=geoname))+
    geom_line(size=1.5) + geom_point(size=2.5) +
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    geom_text(mapping=aes(x=year, y=wages, label=fmt_wages),
              vjust = -0.75, size = 4,  colour="black",
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), label=dollar)+
    scale_x_continuous(breaks=seq(2001,maxYr,3)) +
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("QCEW",""),
         x = "Year",
         y= "Average Weekly Wage") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(size=12),
          legend.position= "bottom")

  f.wages <- merge(f.wagePL_L,f.wageST_L,by="year")
  f.wages <- f.wages[,c(1,5,9)]
  names(f.wages) <- c("Year",paste0(" Average Weekly Wage: ",ctyname), "Average Weekly Wage: Colorado")

  outList <- list("plot" = Plot, "data" = f.wages)


  return(outList)
}
