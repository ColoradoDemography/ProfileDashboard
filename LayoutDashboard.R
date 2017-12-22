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

#setwd("J:/Community Profiles/Shiny Demos")
#setwd("C:/Users/Adam/Documents/Colorado State Demography/Shiny Demos")
rm(list = ls())
library(plyr, quietly=TRUE)
library(tidyverse, quietly=TRUE)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL, quietly=TRUE)
library(rmarkdown)
library(robR)



# validStatuses values for boxes
#primary Blue (sometimes dark blue)
#success Green
#info Blue
#warning Orange
#danger Red


#Start Functions
#1) Utility Functions

#'  popPlace : Populates the input$unit field using information from the PostGres estimates database.
#'  @return a data frame with the placefips, countyfips, placename and totalPopulation 
#'  
#'  @param level identifies the level to be used (State, Plannign Regions, Counties, Municipalities/Places)
#'    taken from the input$level parameter from the dashboard


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
#'   
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
#'   
percent <- function(x, digits = 2, format = "f", ...) {  
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}

#' roundUpNice: rounds numbers to celing or floor based on inputs  used in setting the range for the COC chart
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'   @param x is the input numeric value.  
#'   @param Unit the base value to round around
#'  
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
#' 
## Generates the data download
components_d=function(fips, name,lYr){
  yrLst <- seq(1985,lYr,1)
  x=county_profile(fips, 1985:lYr, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
           naturalIncrease=births-deaths)%>%
    bind_cols(data.frame(County=rep(name, length(yrLst))))%>%
    select(County, year, Births=births, Deaths=deaths, NetMigration=netmigration)
  return(x)
}

#'srcFix Removes the <sup>a</sup> Source Notation in selected tables
#'  @param inTab input table
#'  
srcFix <- function(inTab) {
  outTab <- gsub("<sup>a</sup>","",inTab)
  return(outTab)
}


# 2) Table Production Functions

#' statsTable1 outputes the summary table in the stats section of the dashboard, draws data from the census API
#' @param cty the County  FIPS code, including the state value
#' @param place the Place FIPS Code, including the state value.  
#' @param sYr Start Year
#' @param eYr End year
#' 
statsTable1 <- function(cty,place,sYr,eYr){
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
     hhinc <- codemog_api(data="b19013",db="acs1115", geonum=paste("1", state, ctyfips, sep=""), meta="no")
     MedHHValue <- codemog_api(data="b25077",db="acs1115", geonum=paste("1", state, ctyfips, sep=""), meta="no")
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
    hhinc <- codemog_api(data="b19013",db="acs1115", geonum=paste("1", state, placefips, sep=""), meta="no")
    MedHHValue <- codemog_api(data="b25077",db="acs1115", geonum=paste("1", state, placefips, sep=""), meta="no")
    #muni Coli
    #Cost of Living Index
    coli=county_coli%>%
      filter(countyfips==as.numeric(ctyfips))%>%
      mutate(coli_level=paste(coli, level, sep=", "))%>%
      select(coli_level)
  }
  
  
  popchg <- as.numeric(tPopyr2$totalpopulation) - as.numeric(tPopyr1$totalpopulation)
  

  #state Values
  #Median Household Income  B18140 is the total median earnings...  from the 2012-2016 ACS API
  hhinc_state=codemog_api(data="b19013",db="acs1115", geonum=paste("1", state,  sep=""), meta="no")
  
  #median Househld Value 
  MedHHValue_state=codemog_api(data="b25077",db="acs1115", geonum=paste("1", state,  sep=""), meta="no")
  
 #Preparing table
  outTab <- matrix("",nrow=7,ncol=2)
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
  
  outTab[7,1] <- "Sources: *State Demography Office"
  outTab[7,2] <- "+ U.S. Census Bureau, 2011-2015 American Community Survey"
  

  return(outTab)
}

#' popTable The population table showing the annual growth rate in the Population Section
#' @param cty short FIPS code, without the state code
#' @param ctyname the place name
#' @param sYr Start Year
#' @param eYr End year
#'  
popTable <- function(cty,ctyname,sYr,eYr) { 
  #outputs the population trend table in the population section..
 
  state <- "Colorado"
  cntynum <- as.numeric(cty)
  ctyname <- simpleCap(ctyname)
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
 f.Out <- f.Out[,c(1,2,4,3,5,7,6)]
 names(f.Out) <- c("Year","County","C_Population","C_GrowthRate","State","S_Population","S_GrowthRate")
 f.Out$County <- paste0(f.Out$County," County")
 f.Out$C_GrowthRate <- gsub("NA%","",f.Out$C_GrowthRate)
 f.Out$S_GrowthRate <- gsub("NA%","",f.Out$S_GrowthRate)
 # bind list
 outList <- list("table" = OutTab,"data" = f.Out)
 
  return(outList)
  
}

#' raceTab1 Table showing the percentage values by ethnic/race categories
#'    pulls data from API This table shows a set of histoical comparisons between
#'    the 2000 Census, the 2010 Census and the latest ACS API
#' @param the short FIPS code
#' @param ctyname Place Name
#' @param ACS data depository from the American Community Survey API
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
  
  # Producing Joined File
  raceTmp <- inner_join(p4_00, p9_10)
  
  f.raceFin <- inner_join(raceTmp, f.ACSRace)                                    
  

  
  f.raceFin$Race2 <-   ifelse(f.raceFin$race == "HispanicP","Hispanic",
                         ifelse(f.raceFin$race == "NonHispanicP", "Non-Hispanic",
                         ifelse(f.raceFin$race == "NHWhiteP","Non-Hispanic White",
                         ifelse(f.raceFin$race == "NHBlackP","Non-Hispanic Black",
                         ifelse(f.raceFin$race == "NHAIANP","Non-Hispanic Native American/Alaska Native",
                         ifelse(f.raceFin$race == "NHAsianP","Non-Hispanic Asian",
                         ifelse(f.raceFin$race == "NHNHOPIP","Non-Hispanic Native Hawaiian/Pacific Islander",
                         ifelse(f.raceFin$race == "NHOtherP","Non-Hispanic Other","Non-Hispanic, Two Races"))))))))
 
  m.race <- as.matrix(f.raceFin[c(3:9,1), c(6,3, 4,5)]) #This is the matrix table 
  
  
  #Column Names
  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  ACSName <- paste0("20",substr(ACS,6,7),"[note]")
  names_spaced <- c("Race","2000[note]","2010[note]",ACSName) 
  
  #Span Header
  
  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = (ncol(m.race)-1))
  
  # set vector names 
  names(tblHead) <- c(" ", simpleCap(ctyname))
  
  race_tab <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"', 
          digits=1, 
          row.names=FALSE, 
          align='lrrr', 
          caption="Race Trend",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed", full_width = F) %>%
    column_spec(1, width = "45em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(c("Source; 2000 Census",
                   "Source: 2010 Census",
                   ACSSrc), 
                 notation = "symbol")
  
  race_data <- f.raceFin[,c(1,6,3,4,5)]
  race_data$geoname <- simpleCap(race_data$geoname)
  names(race_data)[2] <- "Race_Cat"
  names(race_data)[5] <- toupper(ACS)
  
  outList <- list("table" = race_tab, "data" = race_data)
  return(outList)
}

#' raceTab2 Table showing the percentage values by ethnic/race categories
#'    pulls data from API This table compares Colorado % to selected geographt
#' @param the short FIPS code
#' @param ctyname Place Name
#' @param ACS data depository from the American Community Survey API
#' 
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
           NHTwo=b03002009,
           HispanicP=(Hispanic/TotalPop),
           NonHispanicP=(NonHispanic/TotalPop),
           NHWhiteP=(NHWhite/TotalPop),
           NHBlackP=(NHBlack/TotalPop),
           NHAIANP=(NHAIAN/TotalPop),
           NHAsianP=(NHAsian/TotalPop),
           NHNHOPIP=(NHNHOPI/TotalPop),
           NHOtherP=(NHOther/TotalPop),
           NHTwoP=(NHTwo/TotalPop))
  
  
  f.ACSRace <- gather(ACSRace2[, c(30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
  
  
  
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
           NHTwo=b03002009,
           HispanicP=(Hispanic/TotalPop),
           NonHispanicP=(NonHispanic/TotalPop),
           NHWhiteP=(NHWhite/TotalPop),
           NHBlackP=(NHBlack/TotalPop),
           NHAIANP=(NHAIAN/TotalPop),
           NHAsianP=(NHAsian/TotalPop),
           NHNHOPIP=(NHNHOPI/TotalPop),
           NHOtherP=(NHOther/TotalPop),
           NHTwoP=(NHTwo/TotalPop))
  
  f.ACSRaces <- gather(ACSRaceS2[, c(30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
  
  
  # Producing Joined File
  
  f.raceFin <- inner_join(f.ACSRace, f.ACSRaces, by="race")                                    
  
  f.raceFin[,1] <-   ifelse(f.raceFin[,1] == "HispanicP","Hispanic",
                            ifelse(f.raceFin[,1] == "NonHispanicP", "Non-Hispanic",
                                   ifelse(f.raceFin[,1] == "NHWhiteP","Non-Hispanic White",
                                          ifelse(f.raceFin[,1] == "NHBlackP","Non-Hispanic Black",
                                                 ifelse(f.raceFin[,1] == "NHAIANP","Non-Hispanic Native American/Alaska Native",
                                                        ifelse(f.raceFin[,1] == "NHAsianP","Non-Hispanic Asian",
                                                               ifelse(f.raceFin[,1] == "NHNHOPIP","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                      ifelse(f.raceFin[,1] == "NHOtherP","Non-Hispanic Other","Non-Hispanic, Two Races"))))))))
  
  # reformatting RaceFin
  f.raceFin[,2] <- percent(f.raceFin[,2]*100)
  f.raceFin[,3] <- percent(f.raceFin[,3]*100)
  
  m.race <- as.matrix(f.raceFin) #This is the matrix table 
  
  #Column Names

  ACSSrc <- paste0("Source: ACS 20",substr(ACS,6,7)," 5-Year Dataset") 
  names_spaced <- c("Race",simpleCap(ctyname),"Colorado") 
  
  race_t <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"', 
          digits=1, 
          row.names=FALSE, 
          align='lrrr', 
          caption="Race Comparison",
          col.names = names_spaced, 
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F) %>%
    column_spec(1, width = "45em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    add_footnote(c(ACSSrc))
  
  
  race_data <- data.frame(f.raceFin)
  names(race_data)[1] <- "Race_Cat"
  names(race_data)[2] <- simpleCap(ctyname)
  names(race_data)[3] <- "Colorado"
  
  
  
  outListR <- list("table" = race_t, "data" = race_data)
  
  return(outListR)
}

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
#' @param endYear The first year in the timeseries Defaults to 2013.
#' @param base Base font size.



county_timeseries=function(fips, beginyear=1990,endYear, base=12){
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)
  
  d=county_profile(fips, beginyear:endYear, "totalpopulation")%>%
    select(countyfips, county, year, totalPopulation=totalpopulation)
  d$county <- paste0(d$county, " County")
  
  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Population", title=paste("Population,", beginyear, "to", max(d$year), sep=" "),
         subtitle = d$county)+
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
#' @param  cName County Name string, from input$unit
#' @param  lYr the last year of the output date range
#' 

cocPlot <- function(fips, cName,lYr,base=12) {
  f.coccty <- components_d(fips=fips, name=cName, lYr = lYr)
  f.cocLong <- gather(f.coccty, key = TypeChange, value=Pop, Births, Deaths, NetMigration)
  f.cocLong$TypeChange <- ifelse(f.cocLong$TypeChange =="NetMigration","Net Migration", f.cocLong$TypeChange)
  
  f.cocLong$TypeChange <- factor(f.cocLong$TypeChange,
                                 levels=c("Births","Deaths", "Net Migration"))
 
  pltTitle <- "Components of Change:\nBirths, Deaths, and Net Migration"
  subTitle <- simpleCap(cName)
  minPop <- roundUpNice(min(f.cocLong$Pop),2500)
  maxPop <- roundUpNice(max(f.cocLong$Pop),2500)
  
  
 cocPlt <-  ggplot(data=f.cocLong,aes(x=year, y=Pop, colour=TypeChange)) +
    geom_line() + 
    geom_point(aes(x=year, y=Pop, colour=TypeChange, shape=TypeChange),size=2) +
    scale_colour_manual("Type of Change", values=c("#82BC00", "#009ADD", "#5C666F")) +
    scale_shape_manual("Type of Change", values=seq(15, 17, 1)) +
    scale_x_continuous(breaks=seq(1985, lYr, 5)) +
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
#' 2015 to 2025.
#' The chart is modified from the original.  Now, we show two bars, one for each series.
#'
#' @param fips is the fips code for the county being examined
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#'
ageForecastPRO=function(fips, stYr, eYr, base=12, agegroup="ten"){
 
  fips=as.numeric(fips)
  
  yrs=c(stYr, eYr)
  
  d=county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    group_by(agecat)%>%
    arrange(countyfips, year)
  
 
  barCol <- c("#00953A","#6EC4E8")
  pltTitle <- paste0("Age Forecast")
  subTitle <- paste0(as.character(d[1,2]), " County: Change in Population by Age: ",stYr," to ",eYr )
  d$year <- as.factor(d$year)
  
  p <- d %>%
    ggplot(aes(x=agecat, y=totalpopulation, fill=year))+
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
 
  dWide <- spread(d, year, totalpopulation)
  names(dWide)[4] <- paste0("totalPop_",stYr)
  names(dWide)[5] <- paste0("totalPop_",eYr)
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

incomePRO=function(fips, fips2="", state="08", state2="08", dbid= "acs1115", base=12){
 
  hhinc1=codemog_api(data="b19001",db=dbid, geonum=paste("1", state, fips, sep=""), meta="no")%>%
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
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))%>%
    separate(geoname, into=c("geoname", "state_name"), sep=",")%>%
    select(-state_name, -group)%>%
    mutate(p=as.numeric(value)/sum(as.numeric(value)))
  
  hhinc2=codemog_api(data="b19001",db=dbid, geonum=paste("1", state2, fips2, sep=""), meta="no")%>%
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
                                                    "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))%>%
    mutate(p=as.numeric(value)/sum(as.numeric(value)))
  
  hhinc=bind_rows(hhinc1, hhinc2)
  
  pltTitle <- "Household Income Distribution"
  subTitle <- as.character(hhinc[1,1])
  srcTitle <- paste0("Source: 20", substr(dbid,6,8)," American Community Survey 5-Year Dataset")
  xTitle <- paste0("Income (in 20",substr(dbid,6,8)," Dollars)")
  
  p=hhinc%>%ggplot(aes(x=cat, y=as.numeric(p), fill=geoname))+
    geom_bar(stat="identity", position="dodge")+
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
 hh_place <- hhinc1[,c(3,2,4)]
 names(hh_place)[1] <- "Income_Cat"
 names(hh_place)[2] <- paste(gsub(" ","_",subTitle),"_Count")
 names(hh_place)[3] <- paste(gsub(" ","_",subTitle),"_Pct")
 
 hh_state <- hhinc2[,c(4,3,5)]
 names(hh_state)[1] <- "Income_Cat"
 names(hh_state)[2] <- "CO_Count"
 names(hh_state)[3] <- "CO_Pct"
 
 dWide <- merge(hh_place,hh_state,by="Income_Cat")
                  
  #bind list
  outList <- list("plot"= p, "data" =  dWide)
  return(outList)
}

#' educPRo Creates a Chart comparing educational attainment of two areas
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
#' 
educPRO <- function(fips, state="08", fips2="", state2="08", dbid="acs1115", base=12){
 
  d13p <- codemog_api(data="b15003",db=dbid,geonum=paste("1",state , fips,sep=""),meta="no")
  d13p[,7:32]=as.numeric(as.character(d13p[,7:32]))
  d13pm <- d13p%>%
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
  
  d13c <- codemog_api(data="b15003",db=dbid,geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13c[,7:32]=as.numeric(as.character(d13c[,7:32]))
  d13cm <- d13c%>%
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
  
  d <- rbind(d13pm,d13cm)%>%
    group_by(geoname)%>%
    mutate(p=value/sum(value))
  
  # Preparing Plot
  pltTitle <- "Educational Attaiment,\nPersons Age 25 and Older "
  subTitle <- as.character(d13pm[1,1])  #The is the county Name...
  srcTitle <- paste0("Source: 20", substr(dbid,6,8)," American Community Survey 5-Year Dataset")
  xTitle <- "Educational Attainment"
  
  p=ggplot(d, aes(x=educcat, y=p, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+#, fill=rgb(31,74,126, max=255))+
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=0))+ coord_flip() +
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
  ed_place <- d13pm[,c(10,9)] %>% mutate(pct=value/sum(value))
  names(ed_place)[1] <- "Education_Cat"
  names(ed_place)[2] <- paste(gsub(" ","_",subTitle),"_Count")
  names(ed_place)[3] <- paste(gsub(" ","_",subTitle),"_Pct")
  
  
  ed_state <- d13cm[,c(10,9)] %>% mutate(p=value/sum(value))
  names(ed_state)[1] <- "Education_Cat"
  names(ed_state)[2] <- "CO_Count"
  names(ed_state)[3] <- "CO_Pct"

  d.wide <- merge(ed_place,ed_state,by="Education_Cat")
  d.wide$Education_Cat <- gsub("\\n","",d.wide$Education_Cat)

  d.wide$N <- ifelse(d.wide$Education_Cat == "Less than High School", 1,
              ifelse(d.wide$Education_Cat == "High School Graduate (or GED)", 2,
              ifelse(d.wide$Education_Cat =="Some College or Associate's Degree", 3,
              ifelse(d.wide$Education_Cat =="Bachelor's Degree", 4,5))))
  d.wide <- d.wide[order(d.wide$N),]                                
  d.wide <- d.wide[,1:5]                              

                                
                        
  #bind list
  outList <- list("plot"= p, "data" =  d.wide)
  
  return(outList)
}
 
#End Functions

# The GLOBAL Variables  Add Additional lists items as sections get defined

#Basic Statistics
stats.obj <<-list()


# Population Change
pop.tab1 <<- list()
pop.plot2 <<- list()
pop.plot3 <<- list()
pop.plot4 <<- list()

#Population Characteristics
popc.plot1 <<- list()
popc.plot2 <<- list()
popc.tab1 <<- list()
popc.tab2 <<- list()


# Structure of user Interface
ui <- dashboardPage( skin="green",
  dashboardHeader(title = span(img(src="ShieldOnly_LRG.png", height = 70, align = "top"),"State Demography Office Community Profile Application"), titleWidth=600),  #dashboardHeader
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
                         "Population, Migration and Natural Increase" = "pop",
                         "Population Characteristics: Income, Education and Race"= "popc",
                         "Housing and Households" = "housing",
                          "Employment and Demographic Forecast"="emply",
                         "Commuting" = "comm",
                         "Employment by Industry"="emplind"),
                        selected =  c("stats","pop","popc",
                                      "housing","emply","comm","emplind")
                       ),
    
    #Action Button
    actionButton("profile","View Profile"),
    actionButton("comparison","View Comparison"),
    actionButton("contact","Contact SDO"),
<<<<<<< HEAD
    downloadButton("singlePDF", label="Output Profile to PDF",class="butt"),
    tags$head(tags$style(".butt{background-color:indianred;} .butt{color: white;}"))
=======
    downloadLink("singlePDF", label="Output Profile to PDF")

>>>>>>> cd07a009037decb30a7b57f3fe1b9c529d22e94e
  ), #dashboardSidebar
  dashboardBody(  tags$head( #Link to CSS...
                  tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
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
                   }
               }
          }
             
    #Generate profile UI objects

     svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
     ln1 <- tags$h1(simpleCap(input$unit))
     outputList <- list(ln1)

     
    #stats; Basic Statistics
    if("stats" %in% input$outChk) {
      stats.text <- tags$h2("Basic Statistics")
      if(input$level == "Counties") {
           stats.tab1 <- statsTable1(fipslist,"",2010,2016)
      }
      if(input$level == "Municipalities/Places") {
        stats.tab1 <- statsTable1(CtyFips,fipslist,2010,2016)
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
      
      stats.box1 <- tabBox(width=6, height=280,
                           tabPanel("Table",tags$div(class="Row1Tab",renderTable({stats.tab1},colnames=FALSE))),
                           tabPanel("Information",Stats.info))
      stats.box2 <- box(width=4, height=280,renderPlot({stats.map},height=270))

 
      
      #building List
      stats.list <- list(stats.box1, stats.box2)
      outputList <- append(outputList,stats.list)
      incProgress()
    }
     
    #pop: Population, Migration and Natural Increase
    if("pop" %in% input$outChk){
      #Generate tables, plots and text...
      pop.text0 <- "Population, Migration and Natural Increase Trends"
      pop.tab1  <<- popTable(substr(fipslist,3,5),input$unit,1990,2016)
      pop.plot2 <<- county_timeseries(fips=as.numeric(substr(fipslist,3,5)),endYear=2016,base=10)
      pop.plot3 <<- ageForecastPRO(fips=as.numeric(substr(fipslist,3,5)),2015,2025,base=10)
      pop.plot4 <<- cocPlot(fips=as.numeric(substr(fipslist,3,5)),cName=input$unit,2016) 

      popb1.info <- tags$div(class="dInfo","The Population Growth Table compares population growth for a place to the State.",tags$br(),
                             "The Population Growth estimates are developed by the State Demography Office.",tags$br(),
                             "To download this table, click on the 'Output PDF' button in the sidebar of the dashboard.",tags$br(),
                             "To download data click on the button below", tags$br(),
                             downloadButton("popb1Data","Download Data"))
      
      popb2.info <- tags$div(class="dInfo","The Population Growth Chart shows the growth the total population for a selected location",tags$br(),
                             "The Population Growth data is developed by the State Demography Office.",tags$br(),
                             "To download the chart and the underlying data click on the buttons below", tags$br(),
                             downloadButton("popb2Plot","Download Chart"),
                             downloadButton("popb2Data","Download Data"))
      
      popb3.info <- tags$div(class="dInfo","The Age Forecast Chart compares the age distribution for selected location at two points in time.",tags$br(),
                            "The Age Forecast data is developed by the State Demography Office.",tags$br(),
                            "To download the chart and the underlying data click on the buttons below", tags$br(),
                            downloadButton("popb3Plot","Download Chart"),
                            downloadButton("popb3Data","Download Data"))
      
      popb4.info <- tags$div(class="dInfo","The Components of Change data shows trends in the number of births, deaths, and net migration for a selected location",tags$br(),
                              "The Components of Change data is developed by the State Demography Office.",tags$br(),
                              "To download the chart and the underlying data click on the buttons below", tags$br(),
                              downloadButton("popb4Plot","Download Chart"),
                              downloadButton("popb4Data","Download Data"))

      
      # Bind to boxes
       pop.box0 <- box(width=12, height= 40, title= pop.text0, 
                        status="primary", solidHeader = TRUE)
       pop.box1 <- tabBox(width=6, height=400,
                          tabPanel("Table",tags$div(class="cleanTab", HTML(srcFix(pop.tab1$table)))),
                          tabPanel("Sources and Downloads",popb1.info))
       pop.box2 <- tabBox(width=6, height=400,
                          tabPanel("Chart", renderPlot({pop.plot2$plot},height=340)),
                          tabPanel("Sources and Downloads",popb2.info))
       pop.box3 <- tabBox(width=6, height=400,
                          tabPanel("Chart",renderPlot({pop.plot3$plot},height=340)),
                          tabPanel("Sources and Downloads", popb3.info))
       pop.box4 <- tabBox(width=6, height=400,
                          tabPanel("Chart",renderPlot({pop.plot4$plot},height=340)),
                          tabPanel("Sources and Downloads",popb4.info))
   
       
       #Append to List
       pop.list <- list(pop.box0,pop.box1,pop.box2,pop.box3,pop.box4)
       outputList <- append(outputList,pop.list)
       incProgress()
    }
    # Population Chatacteristics 
     if("popc" %in% input$outChk){

       #Generate tables, plots and text...
       popc.text0 <- "Population Characteristics: Income, Education and Race"
       popc.plot1 <<- incomePRO(fips=substr(fipslist,3,5))
       popc.plot2 <<- educPRO(fips=substr(fipslist,3,5))
       popc.tab1 <<- raceTab1(fips=substr(fipslist,3,5),ctyname=input$unit,"acs1115") 
       popc.tab2 <<- raceTab2(fips=substr(fipslist,3,5),ctyname=input$unit,"acs1115")
      
       #Contents of Information Tabs
       popcb1.info <- tags$div(class="dInfo","The Income Disctibution Chart compares the distribution of household income for a selected location to the State.",tags$br(),
                               "Information on household Income is taken from the American Community Survey",tags$br(), tags$br(),
                               "To download the chart and the underlying data click on the buttons below", tags$br(),
                               downloadButton("popcb1Plot","Download Chart"),
                               downloadButton("popcb1Data","Download Data"))
       
       popcb2.info <- tags$div(class="dInfo","The Educational Attainment Chart compares the categories of educational attaiment for adults aged 25 and older for a selected location to the State.",tags$br(),
                               "Information on educational attainment is taken from the American Community Survey",tags$br(), tags$br(),
                               "To download the chart and the underlying data click on the buttons below", tags$br(),
                               downloadButton("popcb2Plot","Download Chart"),
                               downloadButton("popcb2Data","Download Data"))
       
       popcb3.info <- tags$div(class="dInfo","The Race Trend Table shows changes in the distribution of racial idenification since the 2000 Census.",tags$br(),
                               "Information on racial identification is taken from the U.S. Census Bureau and the American Community Survey",tags$br(), tags$br(),
                               "To download this table, click on the 'Output PDF' button in the sidebar of the dashboard.",tags$br(), tags$br(),
                               "To download data click on the button below", tags$br(),
                               downloadButton("popcb3Data","Download Data"))
       
       popcb4.info <- tags$div(class="dInfo","The Race Comparison Table compares the distribution of racial idenification of a place to the State.",tags$br(),
                               "Information on racial identification is taken from the American Community Survey",tags$br(), tags$br(),
                               "To download this table, click on the 'Output PDF' button in the sidebar of the dashboard.",tags$br(), tags$br(),
                               "To download data click on the button below", tags$br(),
                               downloadButton("popcb4Data","Download Data"))
       
       # Bind to boxes
       popc.box0 <- box(width=12, height= 40, title= popc.text0, 
                        status="primary", solidHeader = TRUE)
       popc.box1 <- tabBox(width=6, height=400,
                           tabPanel("Chart",renderPlot({popc.plot1$plot},height=340)),
                           tabPanel("Sources and Downloads",popcb1.info))
       popc.box2 <- tabBox(width=6, height=400,
                           tabPanel("Chart",renderPlot({popc.plot2$plot},height=340)),
                           tabPanel("Sources and Downloads",popcb2.info))
       popc.box3 <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popc.tab1$table))),
                           tabPanel("Sources and Downloads",popcb3.info))
       popc.box4 <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popc.tab2$table))),
                           tabPanel("Sources and Downloads",popcb4.info))

       
       #Append to List
       popc.list <- list(popc.box0,popc.box1,popc.box2,popc.box3,popc.box4)
       outputList <- append(outputList,popc.list)
       
       incProgress()
     }
       toggle(id="singlePDF")     
        }) #Progress Bar
         }#if input$unit == ""
      
     # Output UI...
    output$ui <- renderUI(outputList)
    
    
 
  }) #observeEvent input$profile
  
  #Event to output PDF documents
   output$singlePDF <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
             paste0(input$unit," Community Profile ",format(Sys.Date(),"%Y%m%d"), ".pdf")
        },
        content = function(file) {
          
          
          out = knit2pdf('Report.Rnw', clean = TRUE)
          file.rename(out, file) # move pdf to file for downloading
      }) # Output singlePDF
  
   #Event to outload plot and data
   #Population, migration and Natural Increase
   
   # Population Growth Table
   output$popb1Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," PopGrowthComp_Data",".csv")
     },
     content = function(file) {
       write.csv(pop.tab1$data, file, row.names = FALSE)
     }
   ) 
   
   #Population Growth
   output$popb2Plot <- downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," PopGrowth_Plot",".png")
     },
     content = function(file) {
       ggsave(file, plot = pop.plot2$plot, width =8, height=6, units	="in", device = "png")
     }  )
   
   output$popb2Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," PopGrowth_Data",".csv")
     },
     content = function(file) {
       write.csv(pop.plot2$data, file, row.names = FALSE)
     }
   )
   
   #Age Forecast
   output$popb3Plot <- downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," AgeForecast_Plot",".png")
     },
     content = function(file) {
       ggsave(file, plot = pop.plot3$plot, width =8, height=6, units	="in", device = "png")
     }  )
   
   output$popb3Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," AgeForecast_Data",".csv")
     },
     content = function(file) {
       write.csv(pop.plot3$data, file, row.names = FALSE)
     }
   )
   
   #Components of Change
   output$popb4Plot <- downloadHandler(
       filename = function() {
         paste0(simpleCap(input$unit)," ComponentsOfChange_Plot",".png")
       },
       content = function(file) {
         ggsave(file, plot = pop.plot3$plot, width =8, height=6, units	="in", device = "png")
       }  )

   output$popb4Data <-  downloadHandler(
         filename = function() {
           paste0(simpleCap(input$unit)," ComponentsOfChange_Data",".csv")
         },
         content = function(file) {
           write.csv(pop.plot3$data, file, row.names = FALSE)
         }
       )
   
    #Population Characteristics
    #Income Distribution
   output$popcb1Plot <- downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," Income_Plot",".png")
     },
     content = function(file) {
       ggsave(file, plot = popc.plot1$plot, width =8, height=6, units	="in", device = "png")
     }  )
   
   output$popcb1Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," Income_Data",".csv")
     },
     content = function(file) {
       write.csv(popc.plot1$data, file, row.names = FALSE)
     }
   )
   #Educational Attainment
   output$popcb2Plot <- downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," EducAtt_Plot",".png")
     },
     content = function(file) {
       ggsave(file, plot = popc.plot2$plot, width =8, height=6, units	="in", device = "png")
     }  )
   
   output$popcb2Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," EducAtt_Data",".csv")
     },
     content = function(file) {
       write.csv(popc.plot2$data, file, row.names = FALSE)
     }
   ) 
   
   #Race Trend
   output$popcb3Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," RaceTrend_Data",".csv")
     },
     content = function(file) {
       write.csv(popc.tab1$data, file, row.names = FALSE)
     }
   ) 
   
   #Race Comparison
   output$popcb4Data <-  downloadHandler(
     filename = function() {
       paste0(simpleCap(input$unit)," RaceComp_Data",".csv")
     },
     content = function(file) {
       write.csv(popc.tab2$data, file, row.names = FALSE)
     }
   ) 
  }  #server


shinyApp(ui = ui, server = server)
