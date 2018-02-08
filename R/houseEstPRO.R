#' houseEstPRO Produces a plot of the housing projections from 2010 to 2050
#'   from the household_projections data table
#'
#' @param fips is the numeric fips code for county
#' @param ctyname is the cplace name from input$unit
#' @param curYr Ins the current year value
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export
#'
houseEstPRO <- function(fips, ctyname,curYr, base=10) {

  fipsN <- as.numeric(fips)
  state= 0

  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
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

  sqlPlace <- paste0("SELECT * FROM estimates.household_projections WHERE area_code = ",fipsN,";")
  f.hhP <- dbGetQuery(con, sqlPlace)

  f.hhPlace <-  f.hhP[which(f.hhP$household_type_id == 0 & f.hhP$age_group_id == 0),]

  #Closing the connection
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  # Preparing Plot
  f.hhPlace$datatype <- ifelse(f.hhPlace$year <= curYr, "Estimate", "Forecast")
  f.hhPlace$datatype <- factor(f.hhPlace$datatype, levels=c("Estimate","Forecast"))

  pltTitle <- "Total Estimated Households: 2010-2050"
  subTitle <- ctyname
  srcTitle <- "Source: State Demography Office"

  p <- f.hhPlace%>%
    ggplot(aes(x=year, y=total_households, group=datatype))+
    geom_line(aes(linetype=datatype), color="#00953A", size=1.75) +
    labs(x="Year", y="Housing Units", title=pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""))+

    scale_x_continuous(breaks=seq(2010, 2050, 5)) +
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom",legend.title=element_blank())



  f.hhPlace$place <- ctyname
  f.hhPlaceFin <- f.hhPlace[,c(10,9,8)]
  outList <- list("plot" = p,"data" = f.hhPlaceFin)
  return(outList)
}
