#' residentialLF Creates a two plots showing residential labor force participation
#'
#' Plot 1: a line chart showing forcasted trend line for persons in the labor force and
#'     persons age 16 and older.
#' Plor2: a bar chart comparing forcasted labor force participation for a place and Colorado
#'
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphics and associated data sets
#' @export

residentialLF <- function(fips, ctyname, base=10){
  #fips is the 3-digit character string



  LFSQLPL <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code = ",as.numeric(fips), ";")
  LFSQLST <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code = 0;")


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
  f.LFPlace <- dbGetQuery(con, LFSQLPL)
  f.LFState <- dbGetQuery(con, LFSQLST)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  # Summing by Participation Year

  f.LFPlaceSum <- f.LFPlace %>%
    group_by(population_year) %>%
    summarize(LForce = sum(laborforce),
              Pop16P = sum(cni_pop_16pl))


  # Creating 10-year data file
  f.LFPlaceSum$year10 <- f.LFPlaceSum$population_year - (f.LFPlaceSum$population_year %%10)
  f.LFPlaceSum10 <- f.LFPlaceSum %>%
    group_by(year10) %>%
    summarize(LForce = sum(LForce),
              Pop16P = sum(Pop16P)) %>%
    mutate(PctPart = (LForce/Pop16P) * 100)


  f.LFStateSum <- f.LFState %>%
    group_by(population_year) %>%
    summarize(LForce = sum(laborforce),
              Pop16P = sum(cni_pop_16pl))

  # Creating 10-year data file
  f.LFStateSum$year10 <- f.LFStateSum$population_year - (f.LFStateSum$population_year %%10)
  f.LFStateSum10 <- f.LFStateSum %>%
    group_by(year10) %>%
    summarize(LForce = sum(LForce),
              Pop16P = sum(Pop16P)) %>%
    mutate(PctPart = (LForce/Pop16P) * 100)

  # Building Line Chart
  pltTitle <- "Forecast Resident Labor Force and\nPopulation, Age 16 +"


  LFLine <-  ggplot(data=f.LFPlaceSum) +
    geom_line(aes(x=population_year, y=Pop16P, colour= "Population 16 +"), size=1.50) +
    geom_line(aes(x=population_year, y=LForce,color="Labor Force Participation"), size=1.50) +
    scale_colour_manual(" ", values=c("Labor Force Participation" = "#6EC4E8", "Population 16 +" = "#00953A")) +
    scale_x_continuous(breaks=seq(2010,2040, 5)) +
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Population") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")


  #Building Bar Chart
  f.LFPlaceSum10$geoname <- ctyname
  f.LFStateSum10$geoname <- "Colorado"

  f.LFBar <- rbind(f.LFPlaceSum10, f.LFStateSum10)
  pltTitle <- "Forecast Labor Force Participation Rate\n Persons Age 16 +"

  f.LFBar$geoname <- factor(f.LFBar$geoname, levels=c(ctyname, "Colorado"))

  minPart <- ((min(f.LFBar$PctPart)%/%5) * 5) - 5
  maxPart <- ((max(f.LFBar$PctPart)%/%5) * 5) + 5

  LFBar <- f.LFBar %>%
    ggplot(aes(x=year10, y=PctPart, color=geoname))+
    geom_line(size=1.50) +
    scale_y_continuous(limits= c(minPart,maxPart),label=percent, expand = c(0, 0))+
    scale_color_manual(values=c("#6EC4E8","#00953A"), name="Geography") +
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Percentage of Labor Force Participation") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")

  # preparing datasets


  #line data
  f.LFPlaceSum$geoname <- ctyname
  f.LFPlaceSum <- f.LFPlaceSum[,c(5,1,2,3)]

  f.LFPlaceSum[,c(3,4)] <- round(f.LFPlaceSum[,c(3,4)],digits=0)


  names(f.LFPlaceSum) <- c("Place","Year", "Persons in Labor Force", "Persons  Age 16 +")

  # BarData

  f.LFBarData <- merge(f.LFPlaceSum10, f.LFStateSum10, by="year10")
  f.LFBarData <- f.LFBarData[,c(1:4,6:8)]
  f.LFBarData[,c(2,3)] <- round(f.LFBarData[,c(2,3)])
  f.LFBarData[,c(5,6)] <- round(f.LFBarData[,c(5,6)])

  f.LFBarData[,4] <- percent(f.LFBarData[,4])
  f.LFBarData[,7] <- percent(f.LFBarData[,7])

  names(f.LFBarData) <- c("Year",paste0("Persons in Labor Force: ", ctyname), paste0("Persons  Age 16 +: ", ctyname), paste0("Labor Force Participation: ",ctyname),
                          "Persons in Labor Force: Colorado", "Persons  Age 16 +: Colorado", "Labor Force Participation: Colorado")

  outList <- list("plot1" = LFLine, "data1" = f.LFPlaceSum, "plot2" = LFBar, "data2" = f.LFBarData)

  return(outList)
}
