#'  popPlace : Populates the input$unit field using information from the PostGres estimates database.
#'  return a data frame with the placefips, countyfips, placename and totalPopulation
#'
#' @param level identifies the level to be used (State, Planning Regions, Counties, Municipalities/Places)
#'    taken from the input$level parameter from the dashboard
#'
#' @export


popPlace <- function(level) {

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

  if(level == "County") {
    # f.cLookup contains the county records
    f.cLookup <- dbGetQuery(con, "SELECT countyfips, placefips, municipalityname, year, totalpopulation
                            FROM estimates.county_muni_timeseries WHERE year=2016 and placefips = 0;")

    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)

    f.cLookup <- f.cLookup[c(2:nrow(f.cLookup)),]
    return(f.cLookup)
  }
  if(level == "Place") {
    #f.pLookup is the place records, includes records with countyfips 999, which are multi
    #county municipalities

    f.pLookup <- dbGetQuery(con, "SELECT countyfips, placefips, municipalityname, year, totalpopulation
                            FROM estimates.county_muni_timeseries WHERE year=2016 and placefips != 0
                            and placefips != 99990 and countyfips != 999;")

    f.pLookup$municipalityname <- sub(' \\(Part\\)',' ',f.pLookup$municipalityname)


    #f.mLookup is the multi county cities
    f.mLookup <- dbGetQuery(con, "SELECT countyfips, placefips,  year, totalpopulation
                            FROM estimates.county_muni_timeseries WHERE year=2016 and placefips != 0
                            and placefips != 99990 and countyfips = 999;")

    #Closing the connection
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)

    #merging f.pLookup and f.mLookup and updating totalpopulation value
    f.mLookup <- f.mLookup[,c(2,4)]

    f.pLookupFin <- merge(f.pLookup,f.mLookup,by="placefips", all.x=TRUE)

    f.pLookupFin$totalpopulation <- ifelse(is.na(f.pLookupFin$totalpopulation.y),f.pLookupFin$totalpopulation.x,f.pLookupFin$totalpopulation.y)
    f.pLookupFin <- f.pLookupFin[,c(2,1,3,4,7)]
    return(f.pLookupFin)
  }
}

