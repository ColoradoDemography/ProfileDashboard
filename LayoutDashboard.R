#' Community Profile Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, November 2017-January 2018
#' V0.1 November 17, 2017;  Set up basic layout and structure
#' V0.2 November 20, 2017:
#'   link with codemogProfile to produce basic charts
#'   add DOLA logo
#'   Add link to SDO CSS file
#' V0.3 November 28, 2017
#'   fix layout, and white space
#'   scale objects to fit boxes 
#'   error messages and processing clock
#'   complete the layout
#' V0.4 December 5, 2017
#'   PDF Output
#' V0.5 December 13, 2017
#' Add function to access postgres estimates and building place data output
#' Format PDF  
#' #' V0.6 December 21, 2017
#' Add function to output data and charts
#' Format PDF to read objects produced by the dashboard 
#' V.07 January 1, 2018
#' Restructured UI and output 

#setwd("J:/Community Profiles/Shiny Demos")
#setwd("C:/Users/Adam/Documents/Colorado State Demography/Shiny Demos")
rm(list = ls())
library(tidyverse, quietly=TRUE)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL, quietly=TRUE)
library(rmarkdown)
library(robR)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)


# validStatuses values for boxes
#primary Blue (sometimes dark blue)
#success Green
#info Blue
#warning Orange
#danger Red


#Start Functions Will be modified once a project is established
#Utility functions
#1) Utility Functions

#'  popPlace : Populates the input$unit field using information from the PostGres estimates database.
#'  @return a data frame with the placefips, countyfips, placename and totalPopulation 
#'  
#'  @param level identifies the level to be used (State, Plannign Regions, Counties, Municipalities/Places)
#'    taken from the input$level parameter from the dashboard
#'    @return List of fipscodes, place anames and population counts
#'    @export


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


#' simpleCap convers strings to proper case, stolen from Stackoverflow.
#'   @param x input string
#'   @return String formatted in Proper Case
#'   @export
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

#'  listTofips : Produces a vector of FIPS codes from an inpout list of Census County and Plance Name Codes.
#'  @return If the input is a single place,the function will return a single numeric FIPS code. 
#'  @return If the input is multiple places, the function will return a vector of numeric FIPS codes
#' 
#'  @param  df The census place look up file, produced by popPlace.
#'  @param level identifies the level to be used (State, Plannign Regions, Counties, Municipalities/Places)
#'    taken from the input$level parameter from the dashboard
#'  @param inList1 The list of place names,  Comes from input$unit or input$comp2.
#'  @returns the fipscode(s) for a selected data level
#'  @export

listTofips <- function(df, level, inList1){
  # Function to produce a vector of FIPS codes from an input list of names and codes
  
  fipsl <- vector()
  switch(level,
         "State" = {fipsl = "300"},
         "Planning Regions" = {
           switch(inlist1,
                  "1"	= c("08075","08087","08095","08115","08121","08125"),
                  "2"	= c("08069","08123"),
                  "3"	= c("08001","08005","08013","08014","08019","08031","08035","08047","08059"),
                  "4"	= c("08041","08093","08119"),
                  "5"	= c("08017","08039","08063","08073"),
                  "6"	= c("08009","08011","08025","08061","08089","08099"),
                  "7"	= c("08101"),
                  "8"	= c("08003","08021","08023","08079","08105","08109"),
                  "9"	= c("08007","08033","08067","08083","08111"),
                  "10" = c("08029","08051","08053","08085","08091","08113"),
                  "11" = c("08045","08077","08081","08103","08107"),
                  "12" = c("08037","08049","08057","08097","08117"),
                  "13" = c("08015","08027","08043","08065"),
                  "14" = c("08055","08071")
           )
         }, #Planning Regions
         "Counties" = {
           if(length(inList1) == 1) {  #Only one entry
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),1],digits=0, width=3, format="f",flag= "0"))
           } else {
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),1],digits=0, width=3, format="f",flag= "0"))
             for(i in 2:length(inList1)){
               fipsl <- rbind(fipsl, paste0("08",formatC(df[which(df$municipalityname == inList1[i]),1],digits=0, width=3, format="f",flag= "0")))
             }
           } #if
         }, #County
         "Municipalities/Places" = {
           if(length(inList1) == 1) {  #only one entry
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0"))
           } else {
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0"))
             for(i in 1:length(inList1)){
               fipsl <- rbind(fipsl, paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0")))
             }
           } #if
         } #Municipalities/Places
         
  ) #switch
  
  return(fipsl)
} #end listTofips

#' setYrRange Calculates a range of year values, typically in 5-year increments with adjustments
#'   accounting for StartYr and EndYr values that are not multiples of 5....
#'   @param StartYr a numberic variable for the start of the series
#'   @param EndYr a numeric variable for the end of the series
#'   @return a numeric vector of the years between StartYr and EndYr accounting for odd years
#'   @export
setYrRange <- function(StartYr,EndYr) {
  
  sRem <- StartYr %% 5
  eRem <- EndYr %% 5
  
  if((sRem == 0) & (eRem == 0)) { # Regular 5-year sequence
    outSeq <- seq(StartYr,EndYr,5)
  }
  if((sRem != 0) & (eRem == 0)) { # Regular 5-year sequence
    outSeq <- c(StartYr,seq((StartYr+(5-sRem)),EndYr,5))
  }
  if((sRem == 0) & (eRem != 0)) { # Regular 5-year sequence
    outSeq <- c(seq(StartYr,(EndYr-eRem),5),EndYr)
  }
  if((sRem != 0) & (eRem != 0)) { # Regular 5-year sequence
    outSeq <- c(StartYr,seq((StartYr+(5-sRem)),(EndYr-eRem),5),EndYr)
  }
  return(outSeq)
}  #setYrRange

#' percent: utility function  that ourputs numeric value (typically * 100) as a percentage
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'   @param x is the input numeric value.  The percentage value (y/total *100) is claculated outside of the function
#'   @param digits the number of digits to output
#'   @param format input format
#'   @return a numeric string formatted as a percentage
#'   @export
#'   
percent <- function(x, digits = 2, format = "f", ...) {  
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}

#' roundUpNice: rounds numbers to celing or floor based on inputs  used in setting the range for the COC chart
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'   @param x is the input numeric value.  
#'   @param Unit the base value to round around
#'   @return numeric value rounded up to the specific number of digits
#'   @export
roundUpNice <- function(x, Unit) {
  if(x < 0){
    z <- Unit*floor(x/Unit)
  } else {
    z <- Unit*ceiling(x/Unit)
  }
  
  return(z) 
}

#' components_d: the API call to the components of change data
#' This is only for counties
#'   @param fips a numeric fips county code, 300 calls all counties, 0 calls the state  
#'   @name the county/place name
#'   @return a data frame for the components of change chart
#'  @export
#'  
## Generates the data download
components_d=function(fips, name,lYr){
  yrLst <- seq(1990,lYr,1)
  x=county_profile(fips, 1990:lYr, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
           naturalIncrease=births-deaths)%>%
    bind_cols(data.frame(County=rep(name, length(yrLst))))%>%
    select(County, year, Births=births, Deaths=deaths, NetMigration=netmigration)
  return(x)
}



#' tabTitle manages the output of descriptive tabs in the interface
#' @param item the item name in input$outChk
#' @return  Descriptinve string provided in the tabs of the main interface
#' @export
#' 
tabTitle <-function(item) {
  outTitle <- switch(item,
                     "stats" = "Basic Statistics",
                     "popf" = "Population Change",
                     "pop" ="Age",
                     "popc"= "Income, Race and Education",
                     "housing" = "Housing and Households",
                     "comm" = "Commuting",
                     "emply" = "Employment Forecast",
                     "emplind" = "Employment by Industry")
  return(outTitle)
}

#' tabList returns the list items from the outputList  based on the value of input$outChk
#' @param item the item name in input$outChk
#' @returns the content of each panel, drawn form the output lists defined in the GLOBAL section of the code
#' @export

tabList <- function(item){
  
  outList <- list("Section Not Defined")
  if(item == "stats") {
    outList <- stats.list
  }
  if(item == "popf") {
    outList <- popf.list
  }
  if(item == "pop") {
    outList <- popa.list
  }
  if(item == "popc") {
    outList <- popc.list
  }
  return(outList)
}

#' downloadObjUI and downloadObj  File Download Modules
#' downloadObjUI is the UI function that creates the download buttons
#' @param id is the data name and creates the module/namespace ID
#' downloadObj is the server function that facilitates the download
#' @param place is the place name, typically the value of input$unit
#' @dboj is the data object to be output
#' @export 

downloadObjUI <- function(id) {
  
  ns <- NS(id)
  #Identifying data object and type
  dtype <- substr(id,6,9)
  
  #setting button label
  outLabel <- ifelse(dtype== "plot","Download Plot","Download Data")
  
  downloadButton(ns("download"),outLabel)
}

downloadObj <- function(input, output, session, place, oname, dobj) { 
  dname <- substr(oname,1,5)
  dtype <- substr(oname,6,9)
  
  prefix <- switch(dname,
                   "popf1" = "PopGrowthComp",
                   "popf2" = "PopGrowth",
                   "popf3" = "PopForecast",
                   "popf4" = "ComponentsOfChange",
                   
                   "popa1" = "AgeDistribution",
                   "popa2" = "MedianAge",
                   "popa3" = "AgeForecast",
                   "popa4" = "MigrationbyAge",
                   
                   "popc1" = "Income",
                   "popc2" = "EducAtt",
                   "popc3" = "RaceTrend",
                   "popc4" = "RaceComp")
  
  suffix <- ifelse(dtype == "plot","_Plot.png","_Data.csv")
  
  output$download <-  downloadHandler(
    filename = function() {
      paste0(place,prefix,suffix)
    },
    content = function(file) {
      if(suffix == "_Data.csv") {
        write.csv(dobj, file, row.names = FALSE)
      }
      if(suffix == "_Plot.png") {
        ggsave(file, plot = dobj, width =8, height=6, units	="in", device = "png")
      }
    } #content
  ) #DowhloadHandler
} #downloadObj



#' boxContent outputs the HTML code for content and buttons of the various info boxes
#'    need  to put the box text in a HTML() call to render
#' @param topic The short description of the box content
#' @param descr The long description of the Box
#' @param source the data source (SDO, ACS, or CEN)
#' @param stats A T/F value to output information about statistical tests
#' @param bthType Speciifies whether to output a Plot button and Data button ("plot") 
#'          or just a data button ("data")
#' @return  Content and buttons for a specified info box, buttons:
#'          PlotBtn is the button to download a plot in the downloadHandler
#'          DataBtn is the button to download a data file in the downloadHandler  
#' @export
#' 
boxContent <- function(topic, description, source, stats) {
  
  ui1 <- "" #description
  ui2 <- "" #source string
  ui3 <- "" #stats string 1
  ui4 <- "" #stats string 2
  ui5 <- "" #stats string 3
 
  
  ui1 <- tags$div(description, tags$br())
  
  # source
  if(source =="SDO") {
    ui2 <- paste0("Information on ",topic," is taken from the State Demography Office.")
  }
  if(source =="ACS"){
    ui2 <- paste0("Information on ",topic," is taken from the American Community Survey conducted by the U.S. Census Bureau.")
  }
  if(source =="CEN"){
    ui2 <-  paste0("Information on ",topic," is taken from the 2000 and 2010 decennial Census.")
  }
  
  
  
  #stats block
  if(stats == "T") {
    ui3 <- "Estimates of statistically significant differences are calculated at the 90% confidence level."
    ui4 <-  "For more information on the Margin of Error and its use in statistical testing, see:"
    ui5 <- tags$ul(
        tags$li(tags$a(href="https://demography.dola.colorado.gov/demography/understanding-margins-error/","Understanding Margins of Error",target="_blank")), 
        tags$li(tags$a(href="https://www.census.gov/programs-surveys/acs/guidance.html","U.S. Census Bureau American Community Survey Guidance for Data Users",target="_blank"))
      )
    
  }
 
  
  box <- tags$div(ui1, tags$br(),
                  ui2, tags$br(),
                  ui3, tags$br(),
                  ui4, tags$br(),
                  ui5)
  
  
  return(box)
}


# Table Functions
# 2) Table Production Functions

#' statsTable1 outputes the summary table in the stats section of the dashboard, draws data from the census API
#' @param cty the County  FIPS code, including the state value
#' @param place the Place FIPS Code, including the state value.  
#' @param sYr Start Year
#' @param eYr End year
#' @return kable formatted HTML table
#' @export
#' 
statsTable1 <- function(cty,place,sYr,eYr,ACS){
  #outputs the top table in the dashboard
  #Need to restructure this to support muni_est...
  state <- substr(cty,1,2)
  ctyfips <- substr(cty,3,5)
  if(nchar(place) > 0) {
    placefips <- substr(place,3,7)
  }
  
  #Population and Change Rows
  if(nchar(place) == 0) {  #Counties
    tPopyr1 <- county_profile(as.numeric(ctyfips), sYr,"totalpopulation")
    tPopyr2 <- county_profile(as.numeric(ctyfips), eYr,"totalpopulation")
    tJobs <-  county_jobs(fips=as.numeric(ctyfips), year = eYr) #County
    hhinc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    MedHHValue <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    # Poverty Value
    Poverty <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctPoverty <- percent((as.numeric(Poverty$b17001002)/as.numeric(Poverty$b17001001))*100)
    # Percent native
    Native <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctNative <- percent((as.numeric(Native$b05002003)/as.numeric(Native$b05002001))*100)
    #Cost of Living Index
    coli=county_coli%>%
      filter(countyfips==as.numeric(ctyfips))%>%
      mutate(coli_level=paste(coli, level, sep=", "))%>%
      select(coli_level)
  } 
  if(nchar(place) > 0) {  #Places
    tPopyr1 <- muni_est(as.numeric(placefips), sYr,as.numeric(ctyfips),"totalpopulation")
    tPopyr2 <- muni_est(as.numeric(placefips), eYr,as.numeric(ctyfips),"totalpopulation")
    tJobs <-  county_jobs(fips=as.numeric(ctyfips), year = eYr)#County
    hhinc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    MedHHValue <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    #muni Coli
    
    # Poverty Value
    Poverty <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctPoverty <- percent((Poverty$b17001002/Poverty$b17001001))
    
    # Percent native
    Native <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctNative <- percent((native$b05002003/native$b05002001))
    
    #Cost of Living Index
    coli=county_coli%>%
      filter(countyfips==as.numeric(ctyfips))%>%
      mutate(coli_level=paste(coli, level, sep=", "))%>%
      select(coli_level)
  }
  
  
  popchg <- as.numeric(tPopyr2$totalpopulation) - as.numeric(tPopyr1$totalpopulation)
  
  
  #state Values
  #Median Household Income  B18140 is the total median earnings...  from the 2012-2016 ACS API
  hhinc_state=codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  #median Househld Value 
  MedHHValue_state=codemog_api(data="b25077",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  #Preparing table
  outTab <- matrix("",nrow=9,ncol=2)
  outTab[1,1] <- format(as.numeric(tPopyr2$totalpopulation),nsmall=0, big.mark=",")
  outTab[2,1] <- paste0("Population (",eYr,")*")
  
  outTab[1,2] <- format(as.numeric(popchg),nsmall=0, big.mark=",")
  outTab[2,2]  <- paste0("Population Change (",sYr," to ",eYr, ")*")
  
  outTab[3,1] <- format(round(as.numeric(tJobs$totalJobs),digits=0),nsmall=0, big.mark=",")
  # if(nchar(place) == 0) {
  outTab[4,1] <- paste0("County Employment (",eYr,")*")
  # } else {
  #        outTab[4,1] <- paste0("Municipal/Place Employment (",eYr,")*")   
  # }
  
  outTab[3,2] <- coli$coli_level
  outTab[4,2] <- "County Cost of Living Index (CO=100)*"
  outTab[5,1] <- paste0("$",format(as.numeric(hhinc$b19013001),nsmall=0, big.mark=","))
  outTab[6,1] <- paste0("Median Income (Colorado: ","$",format(as.numeric(hhinc_state$b19013001),nsmall=0, big.mark=","),")+")
  
  outTab[5,2] <- paste0("$",format(as.numeric(MedHHValue$b25077001),nsmall=0, big.mark=","))
  outTab[6,2] <- paste0("Median House Value (Colorado: ","$",format(as.numeric(MedHHValue_state$b25077001),nsmall=0, big.mark=","),")+")
  
  outTab[7,1] <- pctPoverty
  outTab[7,2] <- pctNative
  
  outTab[8,1] <- "Percentage of Population with Incomes lower than the Poverty Line+"
  outTab[8,2] <- "Percentage of Population Born in Colorado+"
  
  outTab[9,1] <- "Sources: *State Demography Office"
  outTab[9,2] <- "+U.S. Census Bureau, 2011-2015 American Community Survey"
  
  
  return(outTab)
}

#' popTable The population table showing the annual growth rate in the Population Section
#' @param cty short FIPS code, without the state code
#' @param ctyname the place name
#' @param sYr Start Year
#' @param eYr End year
#' @return kable formatted HTML table
#' @export
#'   
popTable <- function(cty,ctyname,sYr,eYr) { 
  #outputs the population trend table in the population section..
 
  state <- "Colorado"
  cntynum <- as.numeric(cty)
  yrs <- as.character(setYrRange(sYr,eYr))
  #State Population and Growth Rate
  popCO=county_profile(0, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=percent(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=2)),
           Population=comma(totalpopulation))
  #County Population and Growth Rate
  popCounty=county_profile(cntynum, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    arrange(year)%>%
    mutate(name=county,
           year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=percent(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=2)),
           Population=comma(totalpopulation))
  
  # Creating Output Table
  
  f.County <- popCounty[,c(3,5:7)]
  f.CO <- popCO[,c(1,5:7)]
  f.Out <- merge(f.County,f.CO,by="year")
  m.OutTab <- as.matrix(f.Out[,c(1,4,3,7,6)])
  m.OutTab <- gsub("NA%","",m.OutTab)
  names_spaced <- c("Year","Population","Annual Growth<br/>Rate","Population","Annual Growth<br/>Rate") 
  
  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, state = 2)
  
  # set vector names 
  names(tblHead) <- c(" ", ctyname,state)
  
  # Creating Final Table (kable)
  OutTab  <- m.OutTab %>%
    kable(format='html', table.attr='class="myTable"',
          caption = "Population Trend",
          digits=1, 
          row.names=FALSE, 
          align='lccccc', 
          col.names = names_spaced,
          escape = FALSE) %>%
    kable_styling(bootstrap_options = "condensed") %>%
    column_spec(1, bold = T) %>%
    column_spec(2, width = "13em") %>%
    column_spec(3, width ="18em") %>%
    column_spec(4, width = "13em") %>%
    column_spec(5, width = "18em") %>%
    add_header_above(header=tblHead)  %>%
    add_footnote(c("Source: State Demography Office"))
  
  # Creating Final Data Set
  f.Out <- f.Out[,c(1,4,3,7,6)]
  names(f.Out) <- c("Year",paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                    "Population: Colorado","Growth Rate: Colorado")
  
 
  # bind list
  outList <- list("table" = OutTab,"data" = f.Out)
  
  return(outList)
  
}

#' raceTab1 Table showing the percentage values by ethnic/race categories
#'    pulls data from API This table shows a set of histoical comparisons between
#'    the 2000 Census, the 2010 Census and the latest ACS API
#'    
#'    This table does not report MOEs for ACS series, because of the lack of cunsus MOEs...
#'    
#' @param the short FIPS code
#' @param ctyname Place Name
#' @param ACS data depository from the American Community Survey API
#' @return kable formatted HTML table
#' @export
#'  
raceTab1 <- function(fips,ctyname,ACS) {
  state="08"
  #output race tab using pull from API
  
  #call to ACS Race variables 
  
  ACSRace=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  #Converting values to numeric
  ACSRace[,7:ncol(ACSRace)]=as.numeric(as.character(ACSRace[,7:ncol(ACSRace)]))
  
  ACSRace2 <- ACSRace %>%
    select(geoname:b03002012) %>%
    mutate(TotalPop=b03002001, 
           Hispanic=b03002012, 
           NonHispanic=b03002002, 
           NHWhite=b03002003, 
           NHBlack=b03002004,
           NHAIAN=b03002005, 
           NHAsian=b03002006, 
           NHNHOPI=b03002007, 
           NHOther=b03002008, 
           NHTwo=b03002009,
           HispanicP=percent(round(Hispanic/TotalPop, 3)*100, digits = 2),
           NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100, digits = 2),
           NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100, digits = 2),
           NHBlackP=percent(round(NHBlack/TotalPop, 3)*100, digits = 2),
           NHAIANP=percent(round(NHAIAN/TotalPop,3)*100, digits = 2),
           NHAsianP=percent(round(NHAsian/TotalPop,3)*100, digits = 2),
           NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100, digits = 2),
           NHOtherP=percent(round(NHOther/TotalPop,3)*100, digits = 2),
           NHTwoP=percent(round(NHTwo/TotalPop,3)*100, digits = 2))
  
  
  f.ACSRace <- gather(ACSRace2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
  f.ACSRace$geoname <- ctyname
  ACSRow <- data.frame(geoname = ctyname,
                       race = "TotalP",
                       ACS = "100.00%")
  f.ACSRace <- rbind(f.ACSRace,ACSRow)
  
  #call to Census 2010 API Race variables
  p9_10=codemog_api(data="p9", geonum=paste("1", state, fips, sep=""),meta="no")
  p9_10[,7:ncol(p9_10)]=as.numeric(as.character(p9_10[,7:ncol(p9_10)]))
  
  p9_10=p9_10%>%
    select(geoname:p9011)%>%
    mutate(TotalPop=p9001, Hispanic=p9002, NonHispanic=p9003, NHWhite=p9005, NHBlack=p9006,
           NHAIAN=p9007, NHAsian=p9008, NHNHOPI=p9009, NHOther=p9010, NHTwo=p9011,
           HispanicP=percent(round(Hispanic/TotalPop, 3)*100, digits=2),
           NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100, digits=2),
           NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100, digits=2),
           NHBlackP=percent(round(NHBlack/TotalPop, 3)*100, digits=2),
           NHAIANP=percent(round(NHAIAN/TotalPop,3)*100, digits=2),
           NHAsianP=percent(round(NHAsian/TotalPop,3)*100, digits=2),
           NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100, digits=2),
           NHOtherP=percent(round(NHOther/TotalPop,3)*100, digits=2),
           NHTwoP=percent(round(NHTwo/TotalPop,3)*100, digits=2)) %>%
    select(-p9001:-p9011)%>%
    gather(race, Census.2010, HispanicP:NHTwoP, -geoname:-geonum)
  
  p9_10 <- p9_10[,c(1,18,19)]
  p9_10$geoname <- ctyname
  
  CensRow <- data.frame(geoname = ctyname,
                        race = "TotalP",
                        Census.2010 = "100.00%")
  p9_10 <- rbind(p9_10,CensRow)
  
  #Call to Census 2000 API
  p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, fips, sep=""),meta="no")
  p4_00[,7:ncol(p4_00)]=as.numeric(as.character(p4_00[,7:ncol(p4_00)]))
  p4_00=p4_00%>%
    select(geoname:p4011)%>%
    mutate(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
           NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011,
           HispanicP=percent(round(Hispanic/TotalPop, 3)*100, digits=2),
           NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100, digits=2),
           NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100, digits=2),
           NHBlackP=percent(round(NHBlack/TotalPop, 3)*100, digits=2),
           NHAIANP=percent(round(NHAIAN/TotalPop,3)*100, digits=2),
           NHAsianP=percent(round(NHAsian/TotalPop,3)*100, digits=2),
           NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100, digits=2),
           NHOtherP=percent(round(NHOther/TotalPop,3)*100, digits=2),
           NHTwoP=percent(round(NHTwo/TotalPop,3)*100, digits=2))%>%
    select(-p4001:-p4011)%>%
    gather(race, Census.2000, HispanicP:NHTwoP, -geoname:-geonum)
  
  p4_00 <- p4_00[,c(1,18,19)]
  p4_00$geoname <- ctyname
  
  names(CensRow)[3] <- "Census.2000"
  p4_00 <- rbind(p4_00,CensRow)
  
  # Producing Joined File
  raceTmp <- inner_join(p4_00, p9_10)
  
  f.raceFin <- inner_join(raceTmp, f.ACSRace)                                    
  
  
  
  f.raceFin$Race2 <-ifelse(f.raceFin$race == "TotalP","Total Population", 
                           ifelse(f.raceFin$race == "HispanicP","Hispanic",
                                  ifelse(f.raceFin$race == "NonHispanicP", "Non-Hispanic",
                                         ifelse(f.raceFin$race == "NHWhiteP","Non-Hispanic White",
                                                ifelse(f.raceFin$race == "NHBlackP","Non-Hispanic Black",
                                                       ifelse(f.raceFin$race == "NHAIANP","Non-Hispanic Native American/Alaska Native",
                                                              ifelse(f.raceFin$race == "NHAsianP","Non-Hispanic Asian",
                                                                     ifelse(f.raceFin$race == "NHNHOPIP","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                            ifelse(f.raceFin$race == "NHOtherP","Non-Hispanic Other","Non-Hispanic, Two Races")))))))))
  
  m.race <- as.matrix(f.raceFin[c(1:4,6,5,7:10), c(6,3,4,5)]) #This is the matrix table 
  
  
  #Column Names
  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  ACSName <- paste0("20",substr(ACS,6,7),"[note]")
  names_spaced <- c("Race","2000[note]","2010[note]",ACSName) 
  
  #Span Header
  
  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = (ncol(m.race)-1))
  
  # set vector names 
  names(tblHead) <- c(" ", ctyname)
  
  race_tab <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"', 
          digits=1, 
          row.names=FALSE, 
          align='lrrr', 
          caption="Race Trend",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    column_spec(1, width = "45em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(c("Source; 2000 Census",
                   "Source: 2010 Census",
                   ACSSrc), 
                 notation = "symbol")
  
  race_data <- data.frame(m.race)
  race_data$geoname <- ctyname
  race_data <- race_data[,c(5,1:4)]
  names(race_data) <- c("Geography","Race Category","Census 2000", "Census 2010",toupper(ACS))
  
  outList <- list("table" = race_tab, "data" = race_data)
  return(outList)
}

#' raceTab2 Table showing the percentage values by ethnic/race categories
#'    pulls data from API This table compares Colorado % to selected geography
#'    
#'    This table reports the MOEs and a significance test for each series
#'    comparing the percentages from each table...
#'    
#' @param the short FIPS code
#' @param ctyname Place Name
#' @param ACS data depository from the American Community Survey API
#' @return kable formatted HTML table
#' @export

raceTab2 <- function(fips,ctyname,ACS) {
  state="08"
  #output race tab using pull from API
  #call to ACS Race variables 
  
  ACSRace=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  #Converting values to numeric
  ACSRace[,7:ncol(ACSRace)]=as.numeric(as.character(ACSRace[,7:ncol(ACSRace)]))
  
  ACSRace2 <- ACSRace %>%
    select(geoname:b03002012) %>%
    mutate(TotalPop=b03002001, 
           Hispanic=b03002012, 
           NonHispanic=b03002002, 
           NHWhite=b03002003, 
           NHBlack=b03002004,
           NHAIAN=b03002005, 
           NHAsian=b03002006, 
           NHNHOPI=b03002007, 
           NHOther=b03002008, 
           NHTwo=b03002009)
  
  
  f.ACSRace <- gather(ACSRace2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  
  ACSRaceMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  
  ACSRaceMOE[,7:ncol(ACSRaceMOE)]=as.numeric(as.character(ACSRaceMOE[,7:ncol(ACSRaceMOE)]))
  
  ACSRaceMOE2 <- ACSRaceMOE %>%
    select(geoname:b03002_moe012) %>%
    mutate(TotalPop=b03002_moe001, 
           Hispanic=b03002_moe012, 
           NonHispanic=b03002_moe002, 
           NHWhite=b03002_moe003, 
           NHBlack=b03002_moe004,
           NHAIAN=b03002_moe005, 
           NHAsian=b03002_moe006, 
           NHNHOPI=b03002_moe007, 
           NHOther=b03002_moe008, 
           NHTwo=b03002_moe009)
  
  f.ACSRaceMOE_Fin <- gather(ACSRaceMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)  
  
  
  #call to ACS, State Table
  ACSRaceS=codemog_api(data="b03002", db=ACS, geonum=paste("1", state,  sep=""),meta="no")
  #Converting values to numeric
  ACSRaceS[,7:ncol(ACSRaceS)]=as.numeric(as.character(ACSRaceS[,7:ncol(ACSRaceS)]))
  
  ACSRaceS2 <- ACSRaceS %>%
    select(geoname:b03002012) %>%
    mutate(TotalPop=b03002001, 
           Hispanic=b03002012, 
           NonHispanic=b03002002, 
           NHWhite=b03002003, 
           NHBlack=b03002004,
           NHAIAN=b03002005, 
           NHAsian=b03002006, 
           NHNHOPI=b03002007, 
           NHOther=b03002008, 
           NHTwo=b03002009)
  
  f.ACSRaceS <- gather(ACSRaceS2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  # State level MOEs
  
  ACSRaceMOES=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")
  
  ACSRaceMOES[,7:ncol(ACSRaceMOES)]=as.numeric(as.character(ACSRaceMOES[,7:ncol(ACSRaceMOES)]))
  
  ACSRaceMOES2 <- ACSRaceMOES %>%
    select(geoname:b03002_moe012) %>%
    mutate(TotalPop=b03002_moe001, 
           Hispanic=b03002_moe012, 
           NonHispanic=b03002_moe002, 
           NHWhite=b03002_moe003, 
           NHBlack=b03002_moe004,
           NHAIAN=b03002_moe005, 
           NHAsian=b03002_moe006, 
           NHNHOPI=b03002_moe007, 
           NHOther=b03002_moe008, 
           NHTwo=b03002_moe009)
  
  f.ACSRaceMOES_Fin <- gather(ACSRaceMOES2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)  
  
  
  
  # Producing Joined File
  # the place file
  f.place <- merge(f.ACSRace,f.ACSRaceMOE_Fin,by="race")
  names(f.place) <- c("Race","Count_Place","MOE_Place")
  
  total_Place <- as.numeric(f.place[which(f.place$Race == "TotalPop"),2])
  f.place$CountPCT_Place <- f.place$Count_Place/total_Place
  f.place$MOEPCT_Place <- f.place$MOE_Place/total_Place    
  
  # the state file
  f.state <- merge(f.ACSRaceS,f.ACSRaceMOES_Fin,by="race")
  names(f.state) <- c("Race","Count_State","MOE_State")
  
  total_State <- as.numeric(f.state[which(f.state$Race == "TotalPop"),2])
  f.state$CountPCT_State <- f.state$Count_State/total_State
  f.state$MOEPCT_State <- f.state$MOE_State/total_State   
  
  f.raceFin <- merge(f.place,f.state, by="Race")
  
  #Revising the Levels
  f.raceFin[,1] <-   ifelse(f.raceFin[,1] == "TotalPop", "Total Population",
                            ifelse(f.raceFin[,1] == "Hispanic","Hispanic",
                                   ifelse(f.raceFin[,1] == "NonHispanic", "Non-Hispanic",
                                          ifelse(f.raceFin[,1] == "NHWhite","Non-Hispanic White",
                                                 ifelse(f.raceFin[,1] == "NHBlack","Non-Hispanic Black",
                                                        ifelse(f.raceFin[,1] == "NHAIAN","Non-Hispanic Native American/Alaska Native",
                                                               ifelse(f.raceFin[,1] == "NHAsian","Non-Hispanic Asian",
                                                                      ifelse(f.raceFin[,1] == "NHNHOPI","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                             ifelse(f.raceFin[,1] == "NHOther","Non-Hispanic Other","Non-Hispanic, Two Races")))))))))
  
  
  #Calculating the statistical test
  f.raceFin$ZScore <- (abs(f.raceFin$CountPCT_Place - f.raceFin$CountPCT_State)/
                         sqrt((f.raceFin$MOEPCT_Place^2) + (f.raceFin$MOEPCT_State^2)))
  f.raceFin$Sig_Diff <- ifelse(f.raceFin$ZScore < 1,"No","Yes")
  f.raceFin$Sig_Diff <- ifelse(is.na(f.raceFin$Sig_Diff)," ",f.raceFin$Sig_Diff)
  
  #Formatting Percentage Values
  f.raceFin$CountPCT_Place <- percent(f.raceFin$CountPCT_Place*100)
  f.raceFin$MOEPCT_Place <- percent(f.raceFin$MOEPCT_Place*100)   
  f.raceFin$CountPCT_State <- percent(f.raceFin$CountPCT_State*100)
  f.raceFin$MOEPCT_State <- percent(f.raceFin$MOEPCT_State*100)   
  
  
  m.race <- as.matrix(f.raceFin[c(1,9,8,4,3,2,5,6,7,10),c(1,4,5,8,9,11)]) #This is the matrix table 
  
  #Column Names
  
  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  names_spaced <- c("Race","Percentage","Margin of Error","Percentage","Margin of Error","Signficant<br/>Difference?") 
  
  #Span Header
  
  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 1)
  
  # set vector names 
  names(tblHead) <- c(" ", ctyname,"Colorado"," ") 
  
  race_t <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"', 
          digits=1, 
          row.names=FALSE, 
          align='lrrrrr', 
          caption="Race Comparison",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "30em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(c(ACSSrc))
  
  
  race_data <- data.frame(m.race)
  names(race_data)[1] <- "Race Category"
  names(race_data)[2] <- paste0(ctyname,": ","Percentage")
  names(race_data)[3] <- paste0(ctyname,": ","Margin of Error")
  names(race_data)[4] <- "Colorado: Percentage"
  names(race_data)[5] <- "Colorado: Margin of Error"
  names(race_data)[6] <- "Signficant Difference?"
  
  
  
  outListR <- list("table" = race_t, "data" = race_data)
  
  return(outListR)
}

#' medianAgeTab Creates table showing the Median Age by Gender
#' for a selecte lplace and for the state
#' @param fips The County FIPS number (without leading Zeros)
#' @param state the State FIPS code, defaluts to "08" for Colorado.
#' @param ACS a string identifying the input dataset eg: "acs1115"
#' @param ctyname a string identiying the place name
#' @return a kable table and dataset
#' @export

medianAgeTab <- function(fips, ACS, ctyname, state="08"){

  #Local place Age
  medAge <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, fips, sep=""), meta="no")
  medAgeMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, fips, sep=""), meta="no")
  
  medAge2 <- gather(medAge[1,8:10])
  medAge2$key <- c("Total","Male","Female")
  names(medAge2)[2] <- "MedAge_p"
  
  medAge2MOE <- gather(medAgeMOE[1,8:10])
  medAge2MOE$key <- c("Total","Male","Female")
  names(medAge2MOE)[2] <- "MOE_p"
  
  f.localAge <- merge(medAge2, medAge2MOE, by = "key")
  
  #State Age
  medAgeST  <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  medAgeSTMOE  <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  medAgeST2 <- gather(medAgeST[1,8:10])
  medAgeST2$key <- c("Total","Male","Female")
  names(medAgeST2)[2] <- "MedAge_s"
  
  medAgeST2MOE <- gather(medAgeSTMOE[1,8:10])
  medAgeST2MOE$key <- c("Total","Male","Female")
  names(medAgeST2MOE)[2] <- "MOE_s"
  
  f.stateAge <- merge(medAgeST2, medAgeST2MOE, by = "key")
  
  #Creating Copmbined table
  
  f.ageTab <- merge(f.localAge, f.stateAge, by = "key")
  
  
  
  #Calculating significant differences
  f.ageTab$MedAge_p <- as.numeric(f.ageTab$MedAge_p)
  f.ageTab$MOE_p <- as.numeric(f.ageTab$MOE_p)
  f.ageTab$MedAge_s <- as.numeric(f.ageTab$MedAge_s)
  f.ageTab$MOE_s <- as.numeric(f.ageTab$MOE_s)
  
  f.ageTab$ZScore <- (abs(f.ageTab$MedAge_p - f.ageTab$MedAge_s)/
                        sqrt((f.ageTab$MOE_p^2) + (f.ageTab$MOE_p^2)))
  f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
  f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_p < f.ageTab$MedAge_s,"Younger","Older"),"")
  
  m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
  #Column Names
  
  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  names_spaced <- c("Gender","Median Age","Margin of Error","Median Age","Margin of Error","Signficant<br/>Difference?","Difference<br/>from State") 
  
  #Span Header
  
  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 2)
  
  # set vector names 
  names(tblHead) <- c(" ", ctyname,"Colorado"," ") 
  
  age_t <- m.ageTab %>%
    kable(format='html', table.attr='class="cleanTable"', 
          digits=1, 
          row.names=FALSE, 
          align='lrrrrrr', 
          caption="Median Age by Gender  Comparison",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "30em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    column_spec(7, width ="5em") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(c(ACSSrc)) 
  
  #preparint Output data
  f.ageTab2 <- f.ageTab[,c(1:5,7,8)]
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",ctyname), paste0("Margin of Error: ",ctyname), 
                        "Median Age: Colorado", "Margin of Error: Colorado", "Sig. Difference","Difference from State")
  
  outList <- list("table" = age_t, "data" = f.ageTab2)
  return(outList) 
}

#' housePRO  Produces the housing table
#'  CO Housing Unit Table
#'
#'  This function compares housing occupancy and vacancy rates for a place to the state
#'  tenure from the 2000 and 2010 Census data.
#'
#'  @param fips The FIPS of the Place or County to use for the graph
#'  @param ctyname The place Name
#'  @param ACS  The American Community Survey Vintage
#'  @param state  The State FIPS to use.  Defaults to CO.
#' @return kable formatted HTML table
#' @export
#' 

housePRO=function(fips, ctyname, ACS){
  
  # Building ACS Place data table
  f.b25001 <- codemog_api(data="b25001", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  f.b25003 <- codemog_api(data="b25003", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  f.b25004 <- codemog_api(data="b25004", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  
  f.AcsPl <- cbind(f.b25001[,c(1,8)], f.b25003[,8:10],f.b25004[,8:15])
  
  f.AcsPl[,2:13]=as.numeric(as.character(f.AcsPl[,2:13]))
  
  f.AcsPl <- f.AcsPl %>% rename(Total=b25001001, Occupied=b25003001, Vacant=b25004001,
                                Owner = b25003002, Renter = b25003003, Seasonal = b25004006)%>%
    mutate(Other = sum(b25004002, b25004003, b25004004, b25004005, b25004007,b25004008))
  
  f.AcsPlace <- f.AcsPl[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)
  
  
  # Building ACS Place MOE table
  f.b25001_moe <- codemog_api(data="b25001_moe", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  f.b25003_moe <- codemog_api(data="b25003_moe", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  f.b25004_moe <- codemog_api(data="b25004_moe", db=ACS, geonum=paste("1", "08", fips, sep=""),meta="no")
  
  f.AcsPl_moe <- cbind(f.b25001_moe[,c(1,8)], f.b25003_moe[,8:10],f.b25004_moe[,8:15])
  
  f.AcsPl_moe[,2:13]=as.numeric(as.character(f.AcsPl_moe[,2:13]))
  
  f.AcsPl_moe <- f.AcsPl_moe %>% rename(Total=b25001_moe001, Occupied=b25003_moe001, Vacant=b25004_moe001,
                                        Owner = b25003_moe002, Renter = b25003_moe003, Seasonal = b25004_moe006)%>%
    mutate(Other = sqrt(sum(b25004_moe002^2, b25004_moe003^2, b25004_moe004^2, b25004_moe005^2, b25004_moe007^2,b25004_moe008^2)))
  f.AcsPlace_moe <- f.AcsPl_moe[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)
  
  #Finalizing place table
  f.AcsPLFin <- merge(f.AcsPlace,f.AcsPlace_moe, by="var")
  f.AcsPLFin <- f.AcsPLFin[,c(1:3,5)]
  names(f.AcsPLFin) <- c("HCat", "geoname","PL_Value", "PL_MOE")
  #Calculating  proportions
  PL_Tot <- as.numeric(f.AcsPLFin[6,3])
  f.AcsPLFin$PL_VAL_Prop <- f.AcsPLFin$PL_Value/PL_Tot
  f.AcsPLFin$PL_MOE_Prop <- f.AcsPLFin$PL_MOE/PL_Tot
  f.AcsPLFin$PL_VAL_PCT <- percent(f.AcsPLFin$PL_VAL_Prop *100)
  f.AcsPLFin$PL_MOE_PCT <- percent(f.AcsPLFin$PL_MOE_Prop *100)
  
  f.AcsPLFin <- f.AcsPLFin[,c(1:3,5,7,4,6,8)]
  
  # Building ACS state data table
  f.b25001St <- codemog_api(data="b25001", db=ACS, geonum=paste("1", "08",  sep=""),meta="no")
  f.b25003St <- codemog_api(data="b25003", db=ACS, geonum=paste("1", "08",  sep=""),meta="no")
  f.b25004St <- codemog_api(data="b25004", db=ACS, geonum=paste("1", "08",  sep=""),meta="no")
  
  f.AcsSt <- cbind(f.b25001St[,c(1,8)], f.b25003St[,8:10],f.b25004St[,8:15])
  
  f.AcsSt[,2:13]=as.numeric(as.character(f.AcsSt[,2:13]))
  
  f.AcsSt <- f.AcsSt %>% rename(Total=b25001001, Occupied=b25003001, Vacant=b25004001,
                                Owner = b25003002, Renter = b25003003, Seasonal = b25004006)%>%
    mutate(Other = sum(b25004002, b25004003, b25004004, b25004005, b25004007,b25004008))
  
  f.AcsState <- f.AcsSt[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)
  
  
  # Building ACS State MOE table
  f.b25001St_moe <- codemog_api(data="b25001_moe", db=ACS, geonum=paste("1", "08", sep=""),meta="no")
  f.b25003St_moe <- codemog_api(data="b25003_moe", db=ACS, geonum=paste("1", "08", sep=""),meta="no")
  f.b25004St_moe <- codemog_api(data="b25004_moe", db=ACS, geonum=paste("1", "08", sep=""),meta="no")
  
  f.AcsSt_moe <- cbind(f.b25001St_moe[,c(1,8)], f.b25003St_moe[,8:10],f.b25004St_moe[,8:15])
  
  f.AcsSt_moe[,2:13]=as.numeric(as.character(f.AcsSt_moe[,2:13]))
  
  f.AcsSt_moe <- f.AcsSt_moe %>% rename(Total=b25001_moe001, Occupied=b25003_moe001, Vacant=b25004_moe001,
                                        Owner = b25003_moe002, Renter = b25003_moe003, Seasonal = b25004_moe006)%>%
    mutate(Other = sqrt(sum(b25004_moe002^2, b25004_moe003^2, b25004_moe004^2, b25004_moe005^2, b25004_moe007^2,b25004_moe008^2)))
  f.AcsState_moe <- f.AcsSt_moe[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)
  
  
  #Finalizing state table
  f.AcsStFin <- merge(f.AcsState,f.AcsState_moe, by="var")
  f.AcsStFin <- f.AcsStFin[,c(1:3,5)]
  names(f.AcsStFin) <- c("HCat", "geoname","ST_Value", "ST_MOE")
  #Calculating  proportions
  ST_Tot <- as.numeric(f.AcsStFin[6,3])
  f.AcsStFin$ST_VAL_Prop <- f.AcsStFin$ST_Value/ST_Tot
  f.AcsStFin$ST_MOE_Prop <- f.AcsStFin$ST_MOE/ST_Tot
  f.AcsStFin$ST_VAL_PCT <- percent(f.AcsStFin$ST_VAL_Prop *100)
  f.AcsStFin$ST_MOE_PCT <- percent(f.AcsStFin$ST_MOE_Prop *100)
  
  f.AcsStFin <- f.AcsStFin[,c(1:3,5,7,4,6,8)]
  
  # Assembling Combined Tab: f.longtab
  
  f.longTab <- merge(f.AcsPLFin,f.AcsStFin, by="HCat")
  
  # Calculating Statistical Test
  f.longTab$ZScore <- (abs(f.longTab$PL_VAL_Prop - f.longTab$ST_VAL_Prop)/
                         sqrt((f.longTab$PL_MOE_Prop^2) + (f.longTab$ST_MOE_Prop^2)))
  f.longTab$Sig_Diff <- ifelse(f.longTab$ZScore < 1,"No","Yes")
  
  
  f.longTab$HCat <- ifelse(f.longTab$HCat == "Total","Total Housing Units",
                           ifelse(f.longTab$HCat == "Occupied","Occupied Housing Units",
                                  ifelse(f.longTab$HCat == "Owner", "Owner-Occupied Units",
                                         ifelse(f.longTab$HCat == "Renter", "Renter-Occupied Units",
                                                ifelse(f.longTab$HCat == "Vacant", "Vacant Housing Units",
                                                       ifelse(f.longTab$HCat == "Seasonal","Seasonal Units","All Other Rental Units"))))))  
  
  #Reordering Table and prepating output
  
  f.HouseTab <- f.longTab[c(6,1,3,4,5,2,7),c(1,3,5,8,10,12,15,17)]
  f.HouseTab[2] <- comma(f.HouseTab[2])
  f.HouseTab[5] <- comma(f.HouseTab[5])
  f.HouseTab[1,3] <- ""
  f.HouseTab[1,4] <- ""
  f.HouseTab[1,6] <- ""
  f.HouseTab[1,7] <- ""
  f.HouseTab[1,8] <- ""
  
  m.House <- as.matrix(f.HouseTab)
  
  names(f.HouseTab) <- c("Housing Type", paste0("Housing Units: ",ctyname),
                         paste0("Percentage: ",ctyname),paste0("Margin of Error: ",ctyname),
                         "Housing Units: Colorado", "Percentage: Colorado", "Margin of Error: Colorado",
                         "Signficant Difference")
  
  # Setting up table
  
  #Column Names
  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  names_spaced <- c("Housing Type","Count","Percent","Margin of Error","Count","Percent","Margin of Error","Significant Difference?")
  #Span Header
  
  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 3, "Colorado" = 3, " " = 1)
  
  # set vector names 
  names(tblHead1) <- c(" ", ctyname, "Colorado", " ")
  
  
  Housing_tab <- m.House %>%
    kable(format='html', table.attr='class="cleanTable"', 
          row.names=FALSE, 
          align='lrrrrrrr', 
          caption="Housing Comparison",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    column_spec(1, width = "45em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    column_spec(7, width ="5em") %>%
    column_spec(8, width ="5em") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(c(ACSSrc), 
                 notation = "symbol")
  
  outList <- list("table" = Housing_tab, "data" = f.HouseTab)
  return(outList)
}

#Plotting Functions 
#3) Plotting Functions

#' county_timeseries Creates a \code{ggplot2} chart of the population for a CO county
#' This is a replacement for county_ts_chart  Copied from codemogProfile
#'
#' Takes some basic input on the time period and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 1990 to 2040 (beyond 2013 are
#' forecasts).
#' Note: Requires dplyr, ggplot2, ggthemes, scales, and grid R packages.
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endYear The last year in the timeseries Defaults to 2013.
#' @param base Base font size.
#' @return ggplot2 graphic
#' @export



county_timeseries=function(fips, beginyear=1990,endYear, base=12){

  fips=as.numeric(fips)
  
  d=county_profile(fips, beginyear:endYear, "totalpopulation")%>%
    select(countyfips, county, year, totalPopulation=totalpopulation)
  d$county <- paste0(d$county, " County")
  
  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Population", title=paste("Population,", beginyear, "to", max(d$year), sep=" "),
         subtitle = d$county,
         caption = "Source: State Demography Office")+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=90,size=8))
  
  # Bind List
  outList <- list("plot" = p, "data" = d)
  
  return(outList)
}



#' cocPlot: Components of Change Chart, this is a county-level chart, regardless of output level
#'  Contains the call to components_d
#' @param  fips numeric, county-level FIPS code  ** will need to extract county level FIPS code in all calls
#' @param  ctyname County Name string, from input$unit
#' @param  lYr the last year of the output date range
#' @return ggplot2 graphic
#' @export 

cocPlot <- function(fips, ctyname,lYr,base=12) {
  f.coccty <- components_d(fips=fips, name=ctyname, lYr = lYr)
  f.cocLong <- gather(f.coccty, key = TypeChange, value=Pop, Births, Deaths, NetMigration)
  f.cocLong$TypeChange <- ifelse(f.cocLong$TypeChange =="NetMigration","Net Migration", f.cocLong$TypeChange)
  
  f.cocLong$TypeChange <- factor(f.cocLong$TypeChange,
                                 levels=c("Births","Deaths", "Net Migration"))
  
  pltTitle <- "Components of Change:\nBirths, Deaths, and Net Migration"
  subTitle <- ctyname
  minPop <- roundUpNice(min(f.cocLong$Pop),2500)
  maxPop <- roundUpNice(max(f.cocLong$Pop),2500)
  
  
  cocPlt <-  ggplot(data=f.cocLong,aes(x=year, y=Pop, colour=TypeChange)) +
    geom_line() + 
    geom_point(aes(x=year, y=Pop, colour=TypeChange, shape=TypeChange),size=2) +
    scale_colour_manual("Type of Change", values=c("#82BC00", "#009ADD", "#5C666F")) +
    scale_shape_manual("Type of Change", values=seq(15, 17, 1)) +
    scale_x_continuous(breaks=seq(1990, lYr, 5)) +
    scale_y_continuous(breaks=seq(minPop, maxPop, 2500),label=comma)+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = "Source: State Demography Office", 
         x = "Year",
         y= "Population Change") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")
  
  outList <- list("plot" = cocPlt, "data" = f.coccty) 
  
  return(outList)  
}

#' ageForecastPRO Produces a Age Forecast data set and chart
#' Copied from "ms_popage" in codemgprofile, Modified by AB 12/2017
#' Creates a Chart comparing Forecast Population Growth by Age in Colorado.
#'
#' Uses the data from the State Demography Office package codemog to
#' create a graph showing projected population  changes by Age for each Colorado county from
#' 2010 to 2025.
#' The chart is modified from the original.  Now, we show three bars, one for each series.
#'
#' @param fips is the fips code for the county being examined
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export

ageForecastPRO=function(fips, stYr, mYr, eYr, base=12, agegroup="ten"){
  
  fips=as.numeric(fips)
  
  yrs=c(stYr, mYr, eYr)
  
  d=county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    group_by(agecat)%>%
    arrange(countyfips, year)
  
  
  barCol <- c("#82BC00", "#009ADD", "#5C666F")  
  pltTitle <- paste0("Age Forecast")
  subTitle <- paste0(as.character(d[1,2]), " County: Change in Population by Age: ",stYr," to ",eYr )
  names(d)[3] <- "Year"
  d$Year <- as.factor(d$Year)
  d$Year <- factor(d$Year, levels=yrs)
  p <- d %>%
    ggplot(aes(x=agecat, y=totalpopulation, fill=Year))+
    geom_bar(stat="identity",color="black", position = position_dodge(width=0.8)) +
    scale_y_continuous(label=comma)+
    scale_fill_manual(values=barCol) +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = "Source: State Demography Office", 
         x = "Age Group",
         y= "Total Population") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom") 
  
  #Regrouping Data
  
  dWide <- spread(d, Year, totalpopulation)
  names(dWide)[4] <- paste0("totalPop_",stYr)
  names(dWide)[5] <- paste0("totalPop_",mYr) 
  names(dWide)[6] <- paste0("totalPop_",eYr)
  
  dWide[4] <- round(dWide[4], digits = 0)
  dWide[5] <- round(dWide[5], digits = 0)
  dWide[6] <- round(dWide[6], digits = 0)
  
  dWide$county <- paste0(dWide$county," County")
  
  #binding List
  outList <- list("plot"= p, "data" = dWide)
  
  return(outList)
}

#'  IncomePRO Income Distribution Graph
#'  Modified from ms_income in codemogProfile AB 12/2017
#'
#'  This function pulls data and generates a graph of the income distribution
#'  for the areas selected based on the ACS 09-13 5-year File.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'  @param dbid  Data set id, eg: for 2015 Acs: "acd1115"
#'  @return ggplot2 graphic
#'  @export
#' 
incomePRO=function(fips, ctyname, fips2="", state="08", state2="08", dbid= "acs1115", base=12){
  
  hhinc1VAL=codemog_api(data="b19001",db=dbid, geonum=paste("1", state, fips, sep=""), meta="no")%>%
    select(-b19001001)%>%
    gather(var, value, b19001002:b19001017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
    mutate(geoname=str_trim(geoname, side="both"),
           var2=str_sub(var, -2,-1),
           var3=as.numeric(as.character(var2)),
           group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                             15=10;16=11;17=12"))%>%
    group_by(geoname,group)%>%
    summarise(value=sum(as.numeric(value)))%>%
    
    mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                    "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                    "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))
  
  
  # Place MOE
  hhinc1MOE=codemog_api(data="b19001_moe",db=dbid, geonum=paste("1", state, fips, sep=""), meta="no")%>%
    select(-b19001_moe001)%>%
    gather(var, value, b19001_moe002:b19001_moe017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
    mutate(geoname=str_trim(geoname, side="both"),
           var2=str_sub(var, -2,-1),
           var3=as.numeric(as.character(var2)),
           group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                             15=10;16=11;17=12"))%>%
    group_by(geoname,group)%>%
    summarise(value=sum(as.numeric(value)))%>%
    
    mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                    "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                    "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))
  
  names(hhinc1MOE)[3] <- "MOE"
  # Combining Place Level Data File; Calculating percentages
  f.hh1VAL <- hhinc1VAL[, c(4,1,3)]
  f.hh1MOE <- hhinc1MOE[,c(4,3)]
  hhinc1 <- merge(f.hh1VAL, f.hh1MOE, by="cat")
  # Calculating Confidence intervale and Percentage valuse
  f.hhinc1 <- hhinc1 %>%
    mutate(p_propVAL = value/sum(value),
           p_propMOE = MOE/sum(value)) 
  f.hhinc1$geoname <- ctyname
  f.hhinc1$p_ciLOW  <- f.hhinc1$p_propVAL - f.hhinc1$p_propMOE
  f.hhinc1$p_ciHIGH <- f.hhinc1$p_propVAL + f.hhinc1$p_propMOE
  f.hhinc1$p_pctVAL <- percent(f.hhinc1$p_propVAL *100)
  f.hhinc1$p_pctMOE <- percent(f.hhinc1$p_propMOE *100)
  f.hhinc1$p_pctLOW <- percent(f.hhinc1$p_ciLOW *100)
  f.hhinc1$p_pctHIGH <- percent(f.hhinc1$p_ciHIGH *100)
  
  
  #State Value
  hhinc2VAL=codemog_api(data="b19001",db=dbid, geonum=paste("1", state2, fips2, sep=""), meta="no")%>%
    select(-b19001001)%>%
    gather(var, value, b19001002:b19001017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
    mutate(geoname=str_trim(geoname, side="both"),
           var2=str_sub(var, -2,-1),
           var3=as.numeric(as.character(var2)),
           group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                             15=10;16=11;17=12"))%>%
    group_by(geoname,group)%>%
    summarise(value=sum(as.numeric(value)))%>%
    
    mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                    "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                    "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))
  
  #State MOE
  hhinc2MOE=codemog_api(data="b19001_moe",db=dbid, geonum=paste("1", state2, fips2, sep=""), meta="no")%>%
    select(-b19001_moe001)%>%
    gather(var, value, b19001_moe002:b19001_moe017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
    mutate(geoname=str_trim(geoname, side="both"),
           var2=str_sub(var, -2,-1),
           var3=as.numeric(as.character(var2)),
           group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                             15=10;16=11;17=12"))%>%
    group_by(geoname,group)%>%
    summarise(value=sum(as.numeric(value)))%>%
    
    mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                    "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                    "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))
  
  names(hhinc2MOE)[3] <- "MOE"
  # Combining Place Level Data File; Calculating percentages
  f.hh2VAL <- hhinc2VAL[, c(4,1,3)]
  f.hh2MOE <- hhinc2MOE[,c(4,3)]
  hhinc2 <- merge(f.hh2VAL, f.hh2MOE, by="cat")
  # Calculating Confidence intervale and Percentage valuse
  f.hhinc2 <- hhinc2 %>%
    mutate(s_propVAL = value/sum(value),
           s_propMOE = MOE/sum(value)) 
  
  f.hhinc2$s_ciLOW  <- f.hhinc2$s_propVAL - f.hhinc2$s_propMOE
  f.hhinc2$s_ciHIGH <- f.hhinc2$s_propVAL + f.hhinc2$s_propMOE
  f.hhinc2$s_pctVAL <- percent(f.hhinc2$s_propVAL *100)
  f.hhinc2$s_pctMOE <- percent(f.hhinc2$s_propMOE *100)
  f.hhinc2$s_pctLOW <- percent(f.hhinc2$s_ciLOW *100)
  f.hhinc2$s_pctHIGH <- percent(f.hhinc2$s_ciHIGH *100)
  
  #Preparing Chart 
  f.hhinc1p <- f.hhinc1[, c(1,2,5,7,8)]
  names(f.hhinc1p) <- c("Income_Cat","geoname","prop","propLOW","propHIGH")
  f.hhinc2p <- f.hhinc2[, c(1,2,5,7,8)]
  names(f.hhinc2p) <- c("Income_Cat","geoname","prop","propLOW","propHIGH")
  
  hhinc <- rbind( f.hhinc1p,  f.hhinc2p)
  
  hhinc$Income_Cat <- factor(hhinc$Income_Cat, levels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                        "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                        "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                        "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more"))
  hhinc$geoname <- factor(hhinc$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Household Income Distribution"
  subTitle <- ctyname
  srcTitle <- paste0("Source: 20", substr(dbid,6,8)," American Community Survey 5-Year Dataset")
  xTitle <- paste0("Income (in 20",substr(dbid,6,8)," Dollars)")
  
  p=hhinc%>%ggplot(aes(x=Income_Cat, y=prop, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+
    geom_errorbar(aes(ymin=propLOW, ymax=propHIGH),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = srcTitle, 
         x = xTitle,
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")
  
  # Building Output dataset
  hh_place <- f.hhinc1[,c(1,5,6,9:12)]
  
  hh_state <- f.hhinc2[,c(1,5,6,9:12)]
  
  f.dWide <- merge(hh_place,hh_state,by="cat")
  
  #calcualting Statistical Test
  #Calculating the statistical test
  f.dWide$ZScore <- (abs(f.dWide$p_propVAL - f.dWide$s_propVAL)/
                       sqrt((f.dWide$p_propMOE^2) + (f.dWide$s_propMOE^2)))
  f.dWide$Sig_Diff <- ifelse(f.dWide$ZScore < 1,"No","Yes")
  f.dWide$Sig_Diff <- ifelse(is.na(f.dWide$Sig_Diff)," ",f.dWide$Sig_Diff)
  
  f.dwideo <-  f.dWide[,c(1,4:7,10:13,15)]
  
  names(f.dwideo) <- c("Income_Cat",paste0("Percentage: ",ctyname), paste0("Margin of Error: ",ctyname),
                       paste0("Lower 90% Conf Int: ",ctyname),paste0("Upper 90% Conf Int: ",ctyname),
                       "Percentage: Colorado", "Margin of Error: Colorado",
                       "Lower 90% Conf Int: Colorado","Upper 90% Conf Int: Colorado","Significant Difference")
  
  f.dwideo$Income_Cat <- factor(f.dwideo$Income_Cat, levels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                              "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                              "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                              "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more"))
  #bind list
  outList <- list("plot"= p, "data" =  f.dwideo)
  return(outList)
}

#' educPRO Creates a Chart comparing educational attainment of two areas
#' Modified from ms_ed in codemogProfile AB 12/2017
#' Uses the codemog_api function to access ACS data (defaults to 13-5yr) to create a ggplot2 chart for
#' use in profiles.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param fips2 is the second area to be compared Defaults to Blank
#' @param state2 is the state of the second place to be compared, is set to call up CO since fips2 is blank Defaults to 08
#' @param dbid Specifies the ACS data set to be used, defaults to 2011-2015 5-year file
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export

educPRO <- function(fips, ctyname, state="08", fips2="", state2="08", dbid="acs1115", base=12){
  
  #Place Education Value
  d13p <- codemog_api(data="b15003",db=dbid,geonum=paste("1",state , fips,sep=""),meta="no")
  d13p[,7:32]=as.numeric(as.character(d13p[,7:32]))
  d13pVAL <- d13p%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025) %>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))
  
  # Place Education MOE
  d13pm <- codemog_api(data="b15003_moe",db=dbid,geonum=paste("1",state , fips,sep=""),meta="no")
  d13pm[,7:32]=as.numeric(as.character(d13pm[,7:32]))
  
  #Calculating the summary MOE
  d13pMOE <- d13pm %>%
    mutate(ed1=sqrt(b15003_moe002^2+b15003_moe003^2+b15003_moe004^2+b15003_moe005^2+b15003_moe006^2+b15003_moe007^2+
                      b15003_moe008^2+b15003_moe009^2+b15003_moe010^2+b15003_moe011^2+b15003_moe012^2+b15003_moe013^2+
                      b15003_moe014^2+b15003_moe015^2+b15003_moe016^2),
           ed2=sqrt(b15003_moe017^2+b15003_moe018^2),
           ed3=sqrt(b15003_moe019^2+b15003_moe020^2+b15003_moe021^2),
           ed4=b15003_moe022,
           ed5=sqrt(b15003_moe023^2+b15003_moe024^2+b15003_moe025^2)) %>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))
  
  #Preparing data
  names(d13pMOE)[9] <- "MOE"
  d13pVAL2 <- d13pVAL[,c(1,8,10,9)]
  d13pMOE2 <- d13pMOE[,c(8,9)]
  
  d13pF <- merge(d13pVAL2,d13pMOE2,by="EdLevel")
  f.d13pFin <- d13pF %>%
    mutate(p_propVAL = value/sum(value),
           p_propMOE = MOE/sum(value)) 
  
  f.d13pFin$p_ciLOW  <- f.d13pFin$p_propVAL - f.d13pFin$p_propMOE
  f.d13pFin$p_ciHIGH <- f.d13pFin$p_propVAL + f.d13pFin$p_propMOE
  f.d13pFin$p_pctVAL <- percent(f.d13pFin$p_propVAL *100)
  f.d13pFin$p_pctMOE <- percent(f.d13pFin$p_propMOE *100)
  f.d13pFin$p_pctLOW <- percent(f.d13pFin$p_ciLOW *100)
  f.d13pFin$p_pctHIGH <- percent(f.d13pFin$p_ciHIGH *100)
  
  f.d13pFinM <- f.d13pFin[, c(3,2,6,8,9)]
  names(f.d13pFinM)[2] <- ctyname
  names(f.d13pFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")
  
  
  #State Education Values
  d13c <- codemog_api(data="b15003",db=dbid,geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13c[,7:32]=as.numeric(as.character(d13c[,7:32]))
  d13cVAL <- d13c%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    mutate(geoname=stri_replace_all_charclass(geoname, "\\p{WHITE_SPACE}", ""))
  
  
  # state Education MOE
  d13cm <- codemog_api(data="b15003_moe",db=dbid,geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13cm[,7:32]=as.numeric(as.character(d13cm[,7:32]))
  
  #Calculating the summary MOE
  d13cMOE <- d13cm%>%
    mutate(ed1=sqrt(b15003_moe002^2+b15003_moe003^2+b15003_moe004^2+b15003_moe005^2+b15003_moe006^2+b15003_moe007^2+
                      b15003_moe008^2+b15003_moe009^2+b15003_moe010^2+b15003_moe011^2+b15003_moe012^2+b15003_moe013^2+
                      b15003_moe014^2+b15003_moe015^2+b15003_moe016^2),
           ed2=sqrt(b15003_moe017^2+b15003_moe018^2),
           ed3=sqrt(b15003_moe019^2+b15003_moe020^2+b15003_moe021^2),
           ed4=b15003_moe022,
           ed5=sqrt(b15003_moe023^2+b15003_moe024^2+b15003_moe025^2)) %>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))
  
  #Preparing data
  names(d13cMOE)[9] <- "MOE"
  d13cVAL2 <- d13cVAL[,c(1,8,10,9)]
  d13cMOE2 <- d13cMOE[,c(8,9)]
  
  d13cF <- merge(d13cVAL2,d13cMOE2,by="EdLevel")
  f.d13cFin <- d13cF %>%
    mutate(s_propVAL = value/sum(value),
           s_propMOE = MOE/sum(value)) 
  
  f.d13cFin$s_ciLOW  <- f.d13cFin$s_propVAL - f.d13cFin$s_propMOE
  f.d13cFin$s_ciHIGH <- f.d13cFin$s_propVAL + f.d13cFin$s_propMOE
  f.d13cFin$s_pctVAL <- percent(f.d13cFin$s_propVAL *100)
  f.d13cFin$s_pctMOE <- percent(f.d13cFin$s_propMOE *100)
  f.d13cFin$s_pctLOW <- percent(f.d13cFin$s_ciLOW *100)
  f.d13cFin$s_pctHIGH <- percent(f.d13cFin$s_ciHIGH *100)
  
  f.d13cFinM <- f.d13cFin[, c(3,2,6,8,9)]
  names(f.d13cFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")
  
  
  #Preparing Plot dataset
  d <- rbind(f.d13pFinM,f.d13cFinM)
  
  d$Education_Cat <- factor(d$Education_Cat, levels=c("Less than High School",
                                                      "High School Graduate \n(or GED)",
                                                      "Some College or \nAssociate's Degree", "Bachelor's Degree",
                                                      "Graduate or \nProfessional Degree"))
  
  # Preparing Plot
  d$geoname <- factor(d$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Educational Attaiment,\nPersons Age 25 and Older "
  subTitle <- ctyname  #The is the county Name...
  srcTitle <- paste0("Source: 20", substr(dbid,6,8)," American Community Survey 5-Year Dataset")
  xTitle <- "Educational Attainment"
  
  p=ggplot(d, aes(x=Education_Cat, y=prop, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+
    geom_errorbar(aes(ymin=propLOW, ymax=propHIGH),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=0))+ 
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = srcTitle, 
         x = xTitle,
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")
  
  
  # Prepating output data set
  ed_place <- f.d13pFin[, c(3,6,7,10:13)]
  
  ed_state <- f.d13cFin[, c(3,6,7,10:13)]
  
  
  f.dwide <- merge(ed_place,ed_state,by="educcat")
  
  
  
  #calcualting Statistical Test
  #Calculating the statistical test
  f.dwide$ZScore <- (abs(f.dwide$p_propVAL - f.dwide$s_propVAL)/
                       sqrt((f.dwide$p_propMOE^2) + (f.dwide$s_propMOE^2)))
  f.dwide$Sig_Diff <- ifelse(f.dwide$ZScore < 1,"No","Yes")
  f.dwide$Sig_Diff <- ifelse(is.na(f.dwide$Sig_Diff)," ",f.dwide$Sig_Diff)
  
  
  # Preparing Final File
  f.dwideo <-  f.dwide[,c(1,4:7,10:13,15)]
  
  names(f.dwideo) <- c("Education_Cat",paste0("Percentage: ",ctyname), paste0("Margin of Error: ",ctyname),
                       paste0("Lower 90% Conf Int: ",ctyname),paste0("Upper 90% Conf Int: ",ctyname),
                       "Percentage: Colorado", "Margin of Error: Colorado",
                       "Lower 90% Conf Int: Colorado","Upper 90% Conf Int: Colorado","Significant Difference")
  
  f.dwideo$Education_Cat <- gsub("\\n","",f.dwideo$Education_Cat)
  
  f.dwideo <- f.dwideo[c(4,3,5,1,2),]
  
  
  #bind list
  outList <- list("plot"= p, "data" =  f.dwideo)
  
  return(outList)
}

#' PopForecast Creates a Chart showing population and estmates 
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param byr is the first year of the series to be extracted by county_sya (min 1990)
#' @param eyr is the last  year of the series to be extracted by county_sya (max 2050)
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export

PopForecast <- function(fips, ctyname, byr=1990,eyr=2050, base=10) {

  yrs <- seq(byr,eyr,2)
  d <- county_sya(fips, yrs) %>%
    group_by(county, datatype, year) %>%
    summarize(Tot_pop = sum(as.numeric(totalpopulation)))
  
  p=d%>%
    ggplot(aes(x=as.factor(year), y=round(Tot_pop, digits=0), group=datatype))+
    geom_line(aes(linetype=datatype), color="#00953A", size=1.75) +
    labs(x="Year", y="Population", title=paste("Population Forecast,", byr, "to", eyr, sep=" "),
         subtitle = ctyname, 
         caption = "Source: State Demography Office")+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=90,size=8)) +
    theme(legend.title=element_blank())
  
  # Creating Output data file  
  d[4] <- round(d[4],digits=0)
  d$county <- ctyname
  outList <- list("plot" = p,"data" = d)
  return(outList)
}

#' agePlotPRO Creates a Chart comparing The age distribution of a selected place to the state for a simgle year
#'
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param state is the numeric state code , it defaults to 0 in the county_sya call
#' @param yrs is the single year value to be extracted by county_sya
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export

agePlotPRO  <- function(fips, ctyname, state=0, yrs, base=10, agegroup="ten") {

  #Creating Place data File
  f.place =county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    arrange(countyfips, year) %>% 
    mutate(popTot = sum(totalpopulation)) %>%
    group_by(agecat, add=TRUE) %>%
    mutate(age_Pct = percent((totalpopulation/sum(popTot))*100)) %>%
    mutate(age_Prop = (totalpopulation/sum(popTot))*100)
  
  f.place$county <- ctyname
  
  #Creating State Data file
  f.state =county_sya(state, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    arrange(countyfips, year) %>%
    mutate(popTot = sum(totalpopulation)) %>%
    group_by(agecat, add=TRUE) %>%
    mutate(age_Pct = percent((totalpopulation/sum(popTot))*100)) %>%
    mutate(age_Prop = (totalpopulation/sum(popTot))*100)
  
  # Creating Plot data file
  f.AgePlot <- rbind(f.place, f.state)
  
  #Preparint Plot
  barCol <- c("#6EC4E8","#00953A")  
  pltTitle <- paste0("Population Distribution by Age for ",yrs)
  subTitle <- paste0(ctyname, " vs. Colorado" )
  f.AgePlot$county <- factor(f.AgePlot$county, levels=c(ctyname, "Colorado"))
  
  AgePlot <- f.AgePlot %>%
    ggplot(aes(x=agecat, y=age_Prop, fill=county))+
    geom_bar(stat="identity",color="black", position = position_dodge(width=0.8)) +
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=barCol, name="Geography") +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = "Source: State Demography Office", 
         x = "Age Group",
         y= "Percentage of Total Population") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom") 
  
  

  x <- merge(f.place, f.state, by="agecat")
  f.AgePlot2 <- x[,c(1,4,5,7,12,14)]
  f.AgePlot2[3] <- round(f.AgePlot2[3],digits=0)
  f.AgePlot2[5] <- round(f.AgePlot2[5],digits=0)                       
  names(f.AgePlot2) <- c("Age Category", "Year", paste0("Population: ",ctyname), paste0("Population Percentage: ",ctyname),
                         "Population: Colorado", "Population percentage: Colorado")
  
  outList <- list("plot" = AgePlot, "data" = f.AgePlot2)
  return(outList)
}

#' migbyagePRO Creates a Chart showing the 2000-2010 net Migration rate by age
#'
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export
#' 
migbyagePRO <- function(fips, ctyname, base=10) {
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
  
  sqlPlace <- paste0("SELECT fips, county, agegroup, rate0010 FROM data.netmigrbyage WHERE fips = ",fips,";")
  f.migPlace <- dbGetQuery(con, sqlPlace)
  
  sqlState <- paste0("SELECT fips, county, agegroup, rate0010 FROM data.netmigrbyage WHERE fips = ",state,";")
  f.migState <- dbGetQuery(con, sqlState)
  
  #Closing the connection
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)
  
  # Preparing for merge
  f.migPlace[2] <- ctyname
  f.migState[2] <- "Colorado"
  
  
  f.migplot <- rbind(f.migPlace, f.migState)
  names(f.migplot)[2] <- "geoname"
  
  
  
  f.migplot$geoname <- factor(f.migplot$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Net Migration Rate by Age: 2000-2010"
  subTitle <- ctyname
  srcTitle <- "Source: State Demography Office"
  xTitle = "Age Group"
  
  
  
  
  p <- f.migplot %>%ggplot(aes(x=agegroup, y=rate0010, colour=geoname))+
    geom_line(size=1.5) +
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    theme_codemog(base_size=base)+
    geom_hline(yintercept=0, size=1.05) +
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = srcTitle, 
         x = xTitle,
         y= "Net Migration Rate (per 1,000)") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")
  
  
  #Preparing data set
  f.migD <- merge(f.migPlace,f.migState, by="agegroup")
  f.migData <- f.migD[,c(1,4,7)]
  names(f.migData) <- c("5-Year Age Group",paste0("2000-2010 Migration Rate: ",ctyname), "2000-2010 Migration Rate: Colorado")
  
  outList <-list("plot"=p,"data"= f.migData)
  return(outList)
}


#' houseEstPro Produces a plot of the housing projections from 2010 to 2050
#' from the household_projections data table
#'
#' @param fips is the numeric fips code for county                                                      
#' @param ctyname is the cplace name from input$unit
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic
#' @export
#' 
houseEstPRO <- function(fips, ctyname, base=10) {
  
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
  f.hhPlace$datatype <- ifelse(f.hhPlace$year <= 2016, "Estimate", "Forecast")
  f.hhPlace$datatype <- factor(f.hhPlace$datatype, levels=c("Estimate","Forecast"))
  
  pltTitle <- "Total Estimated Housing Units: 2010-2050"
  subTitle <- ctyname
  srcTitle <- "Source: State Demography Office"
  
  p <- f.hhPlace%>%
    ggplot(aes(x=year, y=total_households, group=datatype))+
    geom_line(aes(linetype=datatype), color="#00953A", size=1.75) +
    labs(x="Year", y="Housing Units", title=pltTitle,
         subtitle = ctyname, 
         caption = "Source: State Demography Office")+
    
    scale_x_continuous(breaks=seq(2010, 2050, 5)) +
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom",legend.title=element_blank())
  
  
  
  f.hhPlace$place <- ctyname
  f.hhPlaceFin <- f.hhPlace[,c(10,9,8)]
  outList <- list("plot" = p,"data" = f.hhPlaceFin)
  return(outList) 
}
#End Functions

# The GLOBAL Variables  Add Additional lists items as sections get defined
# Current ACS database
curACS <- "acs1115"
curYr <- 2016

#Basic Statistics
stats.obj <<-list()
stats.list <<- list()

# Population Change
popa1 <<- list()
popa2 <<- list()
popa3 <<- list()
popa4 <<- list()
popa.list <<- list()

#Population Forecast
popf1 <<- list()
popf2 <<- list()
popf3 <<- list()
popf4 <<- list()
popf.list <<- list()


#Population Characteristics
popc1 <<- list()
popc2 <<- list()
popc3 <<- list()
popc4 <<- list()
popc.list <<- list()


# Structure of user Interface
ui <- 
  dashboardPage( skin="green", title= "State Demography Office Community Profile",
  dashboardHeader(title = span(img(src="ShieldOnly_LRG.png", height = 70, align = "top"),"State Demography Office Community Profile"), titleWidth=550),  #dashboardHeader
  dashboardSidebar( width = 300,  useShinyjs(),
    # data level Drop down
    selectInput("level", "Select Data Level" , 
                   choices=c("Select a Data Level","Counties","Municipalities/Places","Planning Regions","State")
                   ),
    
    # profile Unit dropdown
    selectInput("unit", "Select Profile" ,choices=""),
    # Comparison dropdown 1
    selectizeInput("comp", "Select Comparison" ,choices=""),
    # Comparison dropdown 2
    selectizeInput("comp2","Select Custom Comparisons",choices ="", multiple=TRUE),
    #Output Content Checkboxes
    checkboxGroupInput("outChk", "Select the data elements to display:",
                       choices = c("Basic Statistics" = "stats", 
                                   "Population Change" = "popf",
                         "Population Characteristics: Age" = "pop",
                         "Population Characteristics: Income, Education and Race"= "popc",
                         "Housing and Households" = "housing",
                         "Commuting" = "comm",
                         "Employment and Demographic Forecast"="emply",
                         "Employment by Industry"="emplind"),
                        selected =  c("stats","popf","pop","popc",
                                      "housing","comm", "emply","emplind")
                       ),
    
    #Action Button
    actionButton("profile","View Profile"),
    actionButton("comparison","View Comparison"),
    actionButton("contact","Contact SDO"),
    downloadButton("singlePDF", label="Output Profile to PDF",class="butt"),
         tags$head(tags$style(".butt{background-color:indianred;} .butt{color: white;}")
                   )
  ), #dashboardSidebar
  dashboardBody(  tags$head( #Link to CSS...
                  tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),
                  tags$title("State demography Office Community Profile Dashboard")
                  ),
                  tags$style(HTML("
                  .box.box-solid.box-primary>.box-header {
                                  color:#fffff;
                                  background:#C9C6C5
                                  }
                                  
                                  .box.box-solid.box-primary{
                                  color: #ffffff;
                                  border-bottom-color:#C9C6C5;
                                  border-left-color:#C9C6C5;
                                  border-right-color:#C9C6C5;
                                  border-top-color:#C9C6C5;
                                  }   ")),
                fluidRow(uiOutput("ui"))
  ) #dashboardBody

) # dashboardPage/ui


# Server Management Function
server <- function(session,input, output) { 
  #disabling the singlePDF button (the download PDF function)
  toggle(id="singlePDF")

  # updates Dropdown boxes and selects data level and unit
  # Generating lists, will need to refer to these objects to check for location size and for dependencies

  if(!exists("CountyList")) {
    CountyList <- popPlace("County")  
  }
 if(!exists("PlaceList")) {
  PlaceList <- popPlace("Place")
 }
  
  CustomList <- list()
  observeEvent(input$level, ({
    #claears the comp2 dropdown on change
    updateSelectInput(session, "comp2", choices = "")
    if(input$level == "Select a Data Level") { #the initial state of the dropdowns
          outUnit = ""
          outComp = ""
       }
    if(input$level == "Planning Regions") {
          outUnit <- seq(from=1, to=14, by=1)
          outComp <- c("Selected Region Only" ,"State")
       }
    if(input$level == "Counties") {
           outUnit <- unique(as.list(CountyList[,3]))
           outComp <- c("Selected County Only", "Counties in Planning Region", "Custom List of Counties (Select Below)","State")
                                   }
    if(input$level == "Municipalities/Places") {
           outUnit <- unique(as.list(PlaceList[,3]))
           outComp <- c("Selected Municipality/Place Only", "Similar Municipalities/Places", "County", "Custom List of Municipalities/Places (Select Below)", "State")
                                               }
  
    updateSelectInput(session, "unit", choices = outUnit)
    updateSelectInput(session, "comp", choices = outComp)
  }))  #observeEvent input$level
  
  # Event for Comparison selection
  observeEvent(input$comp, {
                 if((input$level == "Counties") && (input$comp == "Custom List of Counties (Select Below)")){
                 # Creating custom list
                 custList <- as.list(CountyList[which(CountyList$municipalityname != input$unit),3])
                 updateSelectInput(session, "comp2", choices = custList)
                 }
                 if((input$level == "Municipalities/Places") && (input$comp == "Custom List of Municipalities/Places (Select Below)")){
                   # Creating custom list
                   custList <- as.list(unique(PlaceList[which(PlaceList$municipalityname != input$unit),3]))
                   updateSelectInput(session, "comp2", choices = custList)
                 }
              }) #observeEvent input$comp

  
   # Event for click on profile button 
  observeEvent(input$profile,  {

    outputList <- list()

    output$ui <- renderUI(outputList)
    #creating the input FIPS list to generate data
    if(input$unit == "") { 
         lnError <- tags$h2("Please specify a Data Level and a Profile to display")
         outputList <- list(lnError)
         }  else { 
           withProgress(message = 'Generating Profile', value = 0, {  # Initialize Progress bar 
          #Building fipslist
         if(input$level == "Counties") {
              fipslist <- listTofips(CountyList,input$level,input$unit)
              PlFilter <- FALSE
         } else {  #This is for all other levels, need to write aggergation functions for regions and states
               fipslist <- listTofips(PlaceList,input$level,input$unit)
               # checking place size for Municipalities/Places, if place size <= 200 report county information
               if(input$level == "Municipalities/Places"){
                   CtyFips <- paste0("08",formatC(PlaceList[which(PlaceList$municipalityname == input$unit),1],digits=0, width=3, format="f",flag= "0"))
                   PopCheck <- as.numeric(PlaceList[which(PlaceList$municipalityname == input$unit),5])
                   if(PopCheck > 200){
                     PlFilter= FALSE
                   } else {
                     PlFilter <- TRUE
                   } #else PLFilter
               }
         }
           
             
    #Generate profile UI objects

     svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
     placeName <- simpleCap(input$unit)
     ln1 <- tags$h1(placeName)

    #stats; Basic Statistics
    if("stats" %in% input$outChk) {
      stats.text <- tags$h2("Basic Statistics")
      if(input$level == "Counties") {
           stats.tab1 <- statsTable1(fipslist,"",2010,2016,curACS)
      }
      if(input$level == "Municipalities/Places") {
        stats.tab1 <- statsTable1(CtyFips,fipslist,2010,2016,curACS)
      }
      stats.map <- cp_countymap(substr(fipslist,3,5)) 
      stats.obj <<- list("table" = stats.tab1,"plot"= stats.map)
      
      Stats.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabl in each display box.",tags$br(),
                             "Note: County data is displayed for municiaplities and places with fewer than 200 people.",tags$br(), tags$br(), 
                             "General information is available here:", tags$br(), 
                             tags$ul(
                               tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Projections",target="_blank")),
                               tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank")
                                       )))
      
      stats.box0 <- box(width=12,ln1)
      stats.box1 <- tabBox(width=8, height=350,
                           tabPanel("Table",tags$div(class="Row1Tab",renderTable({stats.tab1},colnames=FALSE))),
                           tabPanel("Information",Stats.info))
      stats.box2 <- box(width=4, height=350,renderPlot({stats.map},height=270))

      
      #building List
      stats.list <<- list(stats.box0, stats.box1, stats.box2)
      incProgress()
    }
     # Population change
     if("popf" %in% input$outChk){
 
       #Chart/Table Objects
       popf1  <<- popTable(substr(fipslist,3,5),placeName,1990,2016)
       popf2 <<- county_timeseries(fips=as.numeric(substr(fipslist,3,5)),endYear=2016,base=10)
       popf3 <<- PopForecast(fips=as.numeric(substr(fipslist,3,5)), ctyname = placeName)
       popf4 <<- cocPlot(fips=as.numeric(substr(fipslist,3,5)),ctyname=placeName,2016) 
       
       
       #infobox Objects
       popf1.info <- tags$div(boxContent(topic= "Population Growth estimates",
                                        description = "The Population Growth Table compares population growth for a place to the State.",
                                        source = "SDO",
                                        stats = "F"),  
                            downloadObjUI("popf1data")) 
       
       popf2.info <- tags$div(boxContent(topic= "Population Growth data",
                                        description = "The Population Growth Chart shows the growth the total population for a selected location.",
                                        source = "SDO",
                                        stats = "F"),
                             tags$br(), downloadObjUI("popf2plot"), tags$br(),
                             tags$br(), downloadObjUI("popf2data")) 
       
       popf3.info <- tags$div(boxContent(topic= "Population Forecast",
                                        description = "The Population Forecast plot shows the estimated population growth between 1990 and 2025 for the selected county.",
                                        source = "SDO",
                                        stats = "F"),  
                              tags$br(), downloadObjUI("popf3plot"), tags$br(),
                              tags$br(), downloadObjUI("popf3data")) 
  
       popf4.info <- tags$div(boxContent(topic= "Components of Change",
                                        description = "The Components of Change chart shows the estimated births, deaths and net migration values for a selected place betwwen 1990 and the present.",
                                        source = "SDO",
                                        stats = "F"),
                             tags$br(), downloadObjUI("popf4plot"), tags$br(),
                             tags$br(), downloadObjUI("popf4data")) 
       

       # Bind to boxes
       popf1.box <- tabBox(width=6, height=400,
                          tabPanel("Table",tags$div(class="cleanTab", HTML(popf1$table))),
                          tabPanel("Sources and Downloads",popf1.info))
       popf2.box <- tabBox(width=6, height=400,
                          tabPanel("Plot", renderPlot({popf2$plot},height=340)),
                          tabPanel("Sources and Downloads",popf2.info))
       popf3.box <- tabBox(width=6, height=400,
                          tabPanel("Plot",renderPlot({popf3$plot},height=340)),
                          tabPanel("Sources and Downloads", popf3.info))
       popf4.box <- tabBox(width=6, height=400,
                          tabPanel("Plot",renderPlot({popf4$plot},height=340)),
                          tabPanel("Sources and Downloads",popf4.info))
       
       
       #Append to List
       popf.list <<- list(popf1.box,popf2.box,popf3.box,popf4.box)
       incProgress()
       
       
     }  # popf
     
     
    #pop: Population Table, County Time Series, Population by Age, Median Age
    if("pop" %in% input$outChk){
      #Generate tables, plots and text...
     
      popa1 <<- agePlotPRO(fips=as.numeric(substr(fipslist,3,5)), ctyname=placeName, yrs=2016)
      popa2 <<- medianAgeTab(fips=substr(fipslist,3,5), ACS="acs1115", ctyname=placeName)
      popa3 <<- ageForecastPRO(fips=as.numeric(substr(fipslist,3,5)),2010,2015,2025,base=10)
      popa4 <<- migbyagePRO(fips=as.numeric(substr(fipslist,3,5)), ctyname = placeName)
      
      #Info Boxes
      popa1.info <- tags$div(boxContent(topic= "Age data",
                         description = "The Population by Age chart displays age categories a single year.",
                         source = "SDO",
                         stats = "F"), tags$br(),
                         downloadObjUI("popa1plot"), tags$br(), tags$br(),
                         downloadObjUI("popa1data")) 
    
      popa2.info <- tags$div(boxContent(topic= "Median Age data",
                               description = "The Median Age table compares the median age by gender for a location to the state. ",
                               source = "ACS",
                               stats = "T"), tags$br(),
                              downloadObjUI("popa2data")) 
    
      popa3.info <- tags$div(boxContent(topic= "Population Forecast by Age",
                                        description = "The Population Forecast by Age Chart displays the age distribution between 2010 and 2025 .",
                                        source = "SDO",
                                        stats = "F"), tags$br(),
                                        downloadObjUI("popa3plot"), tags$br(), tags$br(),
                                        downloadObjUI("popa3data")) 
      
      popa4.info <- tags$div(boxContent(topic= "Net Migration by Age",
                                        description = "The Net Migration by Age chart compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                        source = "CEN",
                                        stats = "F"), tags$br(),
                                        downloadObjUI("popa4plot"), tags$br(), tags$br(),
                                        downloadObjUI("popa4data")) 
      
      # Bind to boxes
       popa1.box <- tabBox(width=6, height=400,
                          tabPanel("Table",renderPlot({popa1$plot},height=340)),
                          tabPanel("Sources and Downloads",popa1.info))
       popa2.box <- tabBox(width=6, height=400,
                          tabPanel("Plot", tags$div(class="cleanTab", HTML(popa2$table))),
                          tabPanel("Sources and Downloads",popa2.info))
       popa3.box <- tabBox(width=6, height=400,
                          tabPanel("Plot",renderPlot({popa3$plot},height=340)),
                          tabPanel("Sources and Downloads", popa3.info))
       popa4.box <- tabBox(width=6, height=400,
                          tabPanel("Table",renderPlot({popa4$plot},height=340)),
                          tabPanel("Sources and Downloads",popa4.info))
   
       
       #Append to List
       popa.list <<- list(popa1.box,popa2.box,popa3.box,popa4.box)
       incProgress()
    }
      
     
    # Population c Chatacteristics 
     if("popc" %in% input$outChk){

       #Generate tables, plots and text...
       popc1 <<- incomePRO(fips=substr(fipslist,3,5),ctyname=placeName, dbid=curACS)
       popc2 <<- educPRO(fips=substr(fipslist,3,5), ctyname=placeName, dbid=curACS)
       popc3 <<- raceTab1(fips=substr(fipslist,3,5),ctyname=placeName,curACS) 
       popc4 <<- raceTab2(fips=substr(fipslist,3,5),ctyname=placeName,curACS)
      
       #Contents of Information Tabs
       popc1.info <- tags$div(boxContent(topic= "household income",
                                          description = "The Income Disctibution Chart compares the distribution of household income for a selected location to the state.",
                                          source = "ACS",
                                          stats = "T"),  tags$br(),
                                         downloadObjUI("popc1plot"), tags$br(), tags$br(),
                                         downloadObjUI("popc1data"))
       
       popc2.info <- tags$div(boxContent(topic= "education attainment",
                                          description= "The Educational Attainment Chart compares the categories of educational attaiment for adults aged 25 and older for a selected location to the State.",
                                          source = "ACS",
                                          stats = "T"), tags$br(),
                                          downloadObjUI("popc2plot"), tags$br(), tags$br(),
                                          downloadObjUI("popc2data"))
         

       popc3.info <- tags$div(boxContent(topic= "racial identification",
                                         description= "The Race Trend Table shows changes in the distribution of racial idenification since the 2000 Census.",
                                         source = "ACS",
                                         stats = "F"), tags$br(),
                                         downloadObjUI("popc3data"))
         
         popc4.info <- tags$div(boxContent(topic= "racial identification",
                                           description= "The Race Comparison Table compares the distribution of racial idenification of a place to the State.",
                                           source = "ACS",
                                           stats = "T"), tags$br(),
                                          downloadObjUI("popc4data"))
           
           
       # Bind to boxes
       popc1.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popc1$plot},height=340)),
                           tabPanel("Sources and Downloads",popc1.info))
       popc2.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popc2$plot},height=340)),
                           tabPanel("Sources and Downloads",popc2.info))
       popc3.box <- tabBox(width=6, height=500,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popc3$table))),
                           tabPanel("Sources and Downloads",popc3.info))
       popc4.box <- tabBox(width=6, height=500,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popc4$table))),
                           tabPanel("Sources and Downloads",popc4.info))

       
       #Append to List
       popc.list <<- list(popc1.box,popc2.box,popc3.box,popc4.box)
       incProgress()
     }
       toggle(id="singlePDF")     
        }) #Progress Bar
         }#if input$unit == ""
      
     # Output UI...
    output$ui  <- renderUI({
      tabs <- lapply(1:length(input$outChk), function(i) {  # this determines the number of tabs needed
        id <- paste0("tab", i)
        tabPanel(
          title = tabTitle(input$outChk[[i]]), tabList(input$outChk[[i]])
        ) # TabPanel
      })       
      do.call(tabsetPanel, tabs)
    }) #renderUI
    
    
 
  }) #observeEvent input$profile
  
  #Event to output PDF documents
  # report
   output$singlePDF <- downloadHandler(
        filename = function() {
             paste0(input$unit," Community Profile ",format(Sys.Date(),"%Y%m%d"), ".pdf")
        },
        content = function(file) {
          
          
          out = knit2pdf('Report.Rnw', clean = TRUE)
          file.rename(out, file) # move pdf to file for downloading
      }) # Output singlePDF
  
   #Event to outload plot and data
   #Population Forecast
   callModule(downloadObj, id = "popf1data", simpleCap(input$unit), "popf1data", popf1$data)
   
   callModule(downloadObj, id = "popf2plot", simpleCap(input$unit),"popf2plot", popf2$plot)
   callModule(downloadObj, id = "popf2data", simpleCap(input$unit),"popf2data", popf2$data)
   
   callModule(downloadObj, id = "popf3plot", simpleCap(input$unit), "popf3plot", popf3$plot)
   callModule(downloadObj, id = "popf3data", simpleCap(input$unit), "popf3data", popf3$data)
   
   callModule(downloadObj, id = "popf4plot", simpleCap(input$unit), "popf4plot", popf4$plot)
   callModule(downloadObj, id = "popf4data", simpleCap(input$unit), "popf4data", popf4$data)
   
   #Age
   callModule(downloadObj, id = "popa1plot", simpleCap(input$unit),"popa1plot", popa1$plot)
   callModule(downloadObj, id = "popa1data", simpleCap(input$unit),"popa1data", popa1$data)
   
   callModule(downloadObj, id = "popa2plot", simpleCap(input$unit),"popa2plot", popa2$plot)
   callModule(downloadObj, id = "popa2data", simpleCap(input$unit),"popa2data", popa2$data)

   callModule(downloadObj, id = "popa3plot", simpleCap(input$unit), "popa3plot", popa3$plot)
   callModule(downloadObj, id = "popa3data", simpleCap(input$unit), "popa3data", popa3$data)
   
   callModule(downloadObj, id = "popa4plot", simpleCap(input$unit), "popa4plot", popa4$plot)
   callModule(downloadObj, id = "popa4data", simpleCap(input$unit), "popa4data", popa4$data)
 
   #Population Characteristics
   callModule(downloadObj, id = "popc1plot", simpleCap(input$unit),"popc1plot", popc1$plot)
   callModule(downloadObj, id = "popc1data", simpleCap(input$unit),"popc1data", popc1$data)
   
   callModule(downloadObj, id = "popc2plot", simpleCap(input$unit),"popc2plot", popc2$plot)
   callModule(downloadObj, id = "popc2data", simpleCap(input$unit),"popc2data", popc2$data)
   
   callModule(downloadObj, id = "popc3data", simpleCap(input$unit), "popc3data", popc3$data)
   
   callModule(downloadObj, id = "popc4data", simpleCap(input$unit), "popc4data", popc4$data) 
   
   #Housing
   
  }  #server


shinyApp(ui = ui, server = server)
