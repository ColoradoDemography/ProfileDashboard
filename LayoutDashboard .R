#' Community Profile Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, November 2017-March 2018
#' V 0.9 Final sections and PDF Output

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
library(tinytex)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(VennDiagram)
library(gridExtra)
library(ProfileDashboard)


# The GLOBAL Variables  Add Additional lists items as sections get defined
# Current ACS database
curACS <- "acs1216"
curYr <- 2016

#Basic Statistics
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

#Housing and Household Characteristics
poph1 <<- list()
poph2 <<- list()
poph3 <<- list()
poph4 <<- list()
poph5 <<- list() # Housing Values
poph.list <<- list()

#Commuting (Transit)
popt1 <<- list()
popt2 <<- list()  # This is for the jobs and Migration chart
popt.list <<- list()

#Employment by Industry
popei1 <<- list()
popei2 <<- list()
popei3 <<- list()
popei.list <<- list()

#Employment and Demographic Forecast
popem1 <<- list()
popem2 <<- list()
popem3 <<- list()
popem.list <<- list()

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
                         "Employment by Industry"="emplind",
                         "Employment and Demographic Forecast"="emply"
                         ),
                        selected =  c("stats","popf","pop","popc",
                                      "housing","comm", "emplind","emply")
                       ),

    #Action Button
    actionButton("profile","View Profile"),
    actionButton("comparison","View Comparison"),
    actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')"),
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

    #clears the comp2 dropdown on change
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
    outputList <<- list()
    output$ui <- renderUI(outputList)
    #creating the input FIPS list to generate data
    if(input$unit == "") {
         lnError <- tags$h2("Please specify a Data Level and a Profile to display")
         outputList <<- list(lnError)
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
           stats.tab1 <- statsTable1(cty=fipslist,place="",sYr=2010,eYr=2016,ACS=curACS,oType="html")
      }
      if(input$level == "Municipalities/Places") {
        stats.tab1 <- statsTable1(cty=CtyFips,place=fipslist,sYr=2010,eYr=2016,ACS=curACS,oType="html")
      }
      stats.map <- cp_countymap(substr(fipslist,3,5))

      Stats.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabl in each display box.",tags$br(),
                             "Note: County data is displayed for municiaplities and places with fewer than 200 people.",tags$br(), tags$br(),
                             "General information is available here:", tags$br(),
                             tags$ul(
                               tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Projections",target="_blank")),
                               tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank")
                                       )))

      stats.box0 <- box(width=12,ln1)
      stats.box1 <- tabBox(width=8, height=350,
                           tabPanel("Table",tags$div(class="Row1Tab",HTML(stats.tab1))),
                           tabPanel("Information",Stats.info))
      stats.box2 <- box(width=4, height=350,renderPlot({stats.map},height=270))


      #building List
      stats.list <<- list(stats.box0, stats.box1, stats.box2)
      incProgress()
    }
     # Population Forecasts
     if("popf" %in% input$outChk){

       #Chart/Table Objects
       popf1  <<- popTable(substr(fipslist,3,5),placeName,1990,2016,oType="html")
       popf2 <<- county_timeseries(fips=substr(fipslist,3,5),endYear=2016,base=12)
       popf3 <<- popForecast(fips=as.numeric(substr(fipslist,3,5)), ctyname = placeName)
       popf4 <<- cocPlot(fips=as.numeric(substr(fipslist,3,5)),ctyname=placeName,2016)


       #infobox Objects
       popf1.info <- tags$div(boxContent(title= "Population Growth Estimates",
                                         description = "The Population Growth Table compares population growth for a place to the State.",
                                         MSA= "F", stats = "F", table = "T",
                                         urlList = list(c("SDO County Profile Lookup","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                         tags$br(),
                                         downloadObjUI("popf1data"))

       popf2.info <- tags$div(boxContent(title= "Population Growth Data",
                                        description = "The Population Growth Chart shows the growth the total population for a selected location.",
                                        MSA= "F", stats = "F", table = "F",
                                        urlList = list(c("SDO County and Municipal Timeseries","https://demography.dola.colorado.gov/population/data/county-muni-timeseries/")) ),
                                        tags$br(), downloadObjUI("popf2plot"), tags$br(), tags$br(),
                                        downloadObjUI("popf2data"))

       popf3.info <- tags$div(boxContent(title= "Population Forecast",
                                        description = "The Population Forecast plot shows the estimated population growth between 1990 and 2025 for the selected county.",
                                        MSA= "F", stats = "F", table = "F",
                                        urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                        tags$br(), downloadObjUI("popf3plot"), tags$br(), tags$br(),
                                        downloadObjUI("popf3data"))

       popf4.info <- tags$div(boxContent(title= "Components of Change",
                                        description = "The Components of Change chart shows the estimated births, deaths and net migration values for a selected place betwwen 1990 and the present.",
                                        MSA= "F", stats = "F", table = "F",
                                        urlList=list(c("SDO Components of Change Estimates","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/"))),
                                        tags$br(), downloadObjUI("popf4plot"), tags$br(), tags$br(),
                                        downloadObjUI("popf4data"))


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
      popa2 <<- medianAgeTab(fips=substr(fipslist,3,5), ACS=curACS, ctyname=placeName,oType="html")
      popa3 <<- ageForecastPRO(fips=as.numeric(substr(fipslist,3,5)),2010,2015,2025,base=12)
      popa4 <<- migbyagePRO(fips=as.numeric(substr(fipslist,3,5)), ctyname = placeName)

      #Info Boxes
      popa1.info <- tags$div(boxContent(title= "Population by Age",
                         description = "The Population by Age chart displays age categories a single year.",
                         MSA= "F", stats = "F", table = "F",
                         urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                         tags$br(),
                         downloadObjUI("popa1plot"), tags$br(), tags$br(),
                         downloadObjUI("popa1data"))

      popa2.info <- tags$div(boxContent(title= "Median Age Data",
                               description = "The Median Age table compares the median age by gender for a location to the state.",
                               MSA= "F", stats = "T", table = "T",
                               urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                tags$br(),
                              downloadObjUI("popa2data"))

      popa3.info <- tags$div(boxContent(title= "Population Forecast by Age",
                                        description = "The Population Forecast by Age Chart displays the age distribution between 2010 and 2025 .",
                                        MSA= "F", stats = "F", table = "F",
                                        urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                        tags$br(),
                                        downloadObjUI("popa3plot"), tags$br(), tags$br(),
                                        downloadObjUI("popa3data"))

      popa4.info <- tags$div(boxContent(title= "Net Migration by Age",
                                        description = "The Net Migration by Age chart compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                        MSA= "F", stats = "F", table = "F",
                                        urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                        tags$br(),
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
                          tabPanel("Plot",renderPlot({popa4$plot},height=340)),
                          tabPanel("Sources and Downloads",popa4.info))


       #Append to List
       popa.list <<- list(popa1.box,popa2.box,popa3.box,popa4.box)
       incProgress()
    }


    # Population Chatacteristics
     if("popc" %in% input$outChk){

       #Generate tables, plots and text...
       popc1 <<- incomePRO(fips=substr(fipslist,3,5),ctyname=placeName, ACS=curACS)
       popc2 <<- educPRO(fips=substr(fipslist,3,5), ctyname=placeName, ACS=curACS)
       popc3 <<- raceTab1(fips=substr(fipslist,3,5),ctyname=placeName,ACS=curACS,oType="html")
       popc4 <<- raceTab2(fips=substr(fipslist,3,5),ctyname=placeName,ACS=curACS,oType="html")

       #Contents of Information Tabs
       popc1.info <- tags$div(boxContent(title= "Household Income",
                                          description = "The Household Income Disctibution Plot compares the distribution of household income for a selected location to the state.",
                                          MSA= "F", stats = "T", table = "F",
                                          urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                          tags$br(),
                                          downloadObjUI("popc1plot"), tags$br(), tags$br(),
                                          downloadObjUI("popc1data"))

       popc2.info <- tags$div(boxContent(title= "Education Attainment",
                                          description= "The Educational Attainment Plot compares the categories of educational attaiment for adults aged 25 and older for a selected location to the State.",
                                          MSA= "F", stats = "T", table = "F",
                                          urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                          tags$br(),
                                          downloadObjUI("popc2plot"), tags$br(), tags$br(),
                                          downloadObjUI("popc2data"))


       popc3.info <- tags$div(boxContent(title= "Racial Identification Trend",
                                         description= "The Race Trend Table shows changes in the distribution of racial idenification since the 2000 Census.",
                                         MSA= "F", stats = "F", table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                         tags$br(),
                                         downloadObjUI("popc3data"))

         popc4.info <- tags$div(boxContent(title= "Racial Identification Comparison",
                                           description= "The Race Comparison Table compares the distribution of racial idenification of a place to the State.",
                                           MSA= "F", stats = "T", table = "T",
                                           urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                           tags$br(),
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

     # Housing
     if("housing" %in% input$outChk){

       #Generate tables, plots and text...
       poph1 <<- houseEstPRO(fips=substr(fipslist,3,5),ctyname=placeName,curYr=curYr)
       poph2 <<- housePRO(fips=substr(fipslist,3,5), ctyname=placeName, ACS=curACS,oType="html")
       poph3 <<- OOHouse(fips=substr(fipslist,3,5),ctyname=placeName,ACS=curACS,oType="html")
       poph4 <<- RTHouse(fips=substr(fipslist,3,5),ctyname=placeName,ACS=curACS,oType="html")
       poph5 <<- HouseVal(fips=substr(fipslist,3,5),ctyname=placeName,ACS=curACS,oType="html")

       #Contents of Information Tabs
       poph1.info <- tags$div(boxContent(title= "Housing Unit Forecast",
                                         description = "The Housing Unit Forecast displays the forcested number of housing units between 2010 and 2050.",
                                         MSA= "F", stats = "F", table = "F",
                                         urlList = list(c("SDO Housing Unit Projections","https://demography.dola.colorado.gov/housing-and-households/data/household-projections/")) ),
                                         tags$br(),
                              downloadObjUI("poph1plot"), tags$br(), tags$br(),
                              downloadObjUI("poph1data"))

       poph2.info <- tags$div(boxContent(title= "Housing Type Table",
                                         description= "The Housing Type Table compares the categories of housing types fro a selected place to the State.",
                                         MSA= "F", stats = "T",  table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                         tags$br(),
                              downloadObjUI("poph2data"))


       poph3.info <- tags$div(boxContent(title= "Characteristics of Owner-Occupied Housing",
                                         description= "The Owner-Occupied Housing Table displays the characteristics of owner-occupied housing in a selected place.",
                                         MSA= "F", stats = "F", table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                         tags$br(),
                                         downloadObjUI("poph3data"))

       poph4.info <- tags$div(boxContent(title= "Characteristics of Rental Housing",
                                         description= "The Rental Housing Table displays the characteristics of rental housing in a selected place.",
                                         MSA= "F", stats = "F",table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                         tags$br(),
                                         downloadObjUI("poph4data"))
       poph5.info <- tags$div(boxContent(title= "Comparative Owner-Occupied Housing Values",
                                         description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                         MSA= "F", stats = "T",table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                              tags$br(),
                              downloadObjUI("poph5data"))
       poph6.info <- tags$div(boxContent(title= "Comparative Rental Housing Values",
                                         description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                         MSA= "F", stats = "T",table = "T",
                                         urlList = list(c("American Community Survey American Fact Finder","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                              tags$br(),
                              downloadObjUI("poph6data"))


       # Bind to boxes
       poph1.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({poph1$plot},height=340)),
                           tabPanel("Sources and Downloads",poph1.info))
       poph2.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(poph2$table))),
                           tabPanel("Sources and Downloads",poph2.info))
       poph5.box <- tabBox(width=6, height = 350,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(poph5$table0))),
                           tabPanel("Sources and Downloads",poph5.info))
       poph6.box <- tabBox(width=6, height = 350,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(poph5$tableR))),
                           tabPanel("Sources and Downloads",poph6.info))
       poph3.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(poph3$table))),
                           tabPanel("Sources and Downloads",poph3.info))
       poph4.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(poph4$table))),
                           tabPanel("Sources and Downloads",poph4.info))


       #Append to List
       poph.list <<- list(poph1.box,poph2.box, poph5.box, poph6.box, poph3.box,poph4.box)
       incProgress()
     }

     # Commuting
     if("comm" %in% input$outChk){

       #Generate tables, plots and text...
       popt1 <<- GenerateVenn(fips=substr(fipslist,3,5),ctyname=placeName,oType="html")
       popt2 <<- jobMigration(fips=substr(fipslist,3,5),ctyname=placeName,maxyr = curYr)

       #Contents of Information Tabs
       popt1.info <- tags$div(boxContent(title= "Commuting Patterns Plot",
                                         description = "The Communting Patterns plot shows the number of people working and living in a specified location.",
                                         MSA= "F", stats = "F",  table = "F",
                                         urlList = list(c("LODES On the Map Data","https://onthemap.ces.census.gov/")) ),
                                         tags$br(),
                                         downloadObjUI("popt1plot"))

       popt2.info <- tags$div(boxContent(title= "Work Outside Table",
                                         description= "The work outside table shows the top ten work locations for prople living in an area but working somewhere else.",
                                         MSA= "F", stats = "F", table = "T",
                                         urlList = list(c("LODES On the Map Data","https://onthemap.ces.census.gov/")) ),
                                         tags$br(),
                                         downloadObjUI("popt2data"))


       popt3.info <- tags$div(boxContent(title= "Live Outside Table",
                                         description= "The live outside table shows the top ten residential locations for people working in an area but living somewhere else.",
                                         MSA= "F", stats = "F", table = "T",
                                         urlList = list(c("LODES On the Map Data","https://onthemap.ces.census.gov/")) ),
                                         tags$br(),
                                         downloadObjUI("popt3data"))

       popt4.info <- tags$div(boxContent(title= "Jobs and Net Migration Plot",
                                         description= "The jobs and net migration plot shows the trend between jobs and net migration for a selected place.",
                                         MSA= "F", stats = "F", table = "F",
                                         urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/"),
                                                        c("Bureau of Economic Analysis Jobs Data","https://www.bea.gov/index.htm"))),
                                         tags$br(),
                                         downloadObjUI("popt4plot"),
                                         tags$br(), tags$br(),
                                         downloadObjUI("popt4data"))

       # Bind to boxes

       popt1.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({grid.draw(popt1$plot)},height=340)),
                           tabPanel("Sources and Downloads",popt1.info))
       popt2.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popt1$tab1))),
                           tabPanel("Sources and Downloads",popt2.info))
       popt3.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popt1$tab2))),
                           tabPanel("Sources and Downloads",popt3.info))
       popt4.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popt2$plot},height=340)),
                           tabPanel("Sources and Downloads",popt4.info))

       #Append to List
       popt.list <<- list(popt1.box,popt2.box,popt3.box,popt4.box)
       incProgress()
     }  #Commuting

     #Employment by Industry
     if("emplind" %in% input$outChk){
       #Generate tables, plots and text...
       popei1 <<- ProfileDashboard::ms_jobs(fips=substr(fipslist,3,5),ctyname=placeName, maxyr = curYr)
       popei2 <<- jobsByIndustry(fips=substr(fipslist,3,5),ctyname=placeName, curyr = curYr)
       popei3 <<- baseIndustries(fips=substr(fipslist,3,5),ctyname=placeName, curyr = curYr, oType="html")

       #Contents of Information Tabs
       popei1.info <- tags$div(boxContent(title= "Jobs Forecast",
                                         description = "The Jobs Forecast Plot shows the estimated number of jobs to 2040.",
                                         MSA= "F", stats = "F", table = "F",
                                         urlList = list(c("SDO Labor Force Participation Data","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                         tags$br(),
                                         downloadObjUI("popei1plot"),
                                         tags$br(), tags$br(),
                                         downloadObjUI("popei1data"))

       popei2.info <- tags$div(boxContent(title= "Jobs by Industry",
                                         description= "The Share of Jobs by Industry plot shows the percentage of jobs by major occupational category. ",
                                         MSA= "F", stats = "F", table = "F",
                                         urlList = list(c("SDO Jobs by Industry Sector","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                         tags$br(),
                                         downloadObjUI("popei2plot"),
                                         tags$br(),tags$br(),
                                         downloadObjUI("popei2data"))


       popei3.info <- tags$div(boxContent(title= "Base Industries Plot",
                                         description= "The Base Industries plot shows the percentage distribution of base Indisutries in a selected place.",
                                         MSA= "T", stats = "F", table = "F",
                                         urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                               c("SDO Base industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                         tags$br(),
                                         downloadObjUI("popei3plot"),
                                         tags$br(), tags$br(),
                                         downloadObjUI("popei3data"))

       popei4.info <- tags$div(boxContent(title= "Base Industries Table",
                                         description= "The Base Industries Table summarizes the number of jobs in indirect basic employment, direct basic employment and local services sectors.",
                                         MSA= "T", stats = "F",                                          table = "T",
                                         urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                              c("SDO Base Industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                         tags$br(),
                                         downloadObjUI("popei4data"))
       # Bind to boxes
       popei1.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popei1$plot},height=340)),
                           tabPanel("Sources and Downloads",popei1.info))
       popei2.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popei2$plot},height=340)),
                           tabPanel("Sources and Downloads",popei2.info))
       popei3.box <- tabBox(width=6, height=400,
                           tabPanel("Plot",renderPlot({popei3$plot},height=340)),
                           tabPanel("Sources and Downloads",popei3.info))
       popei4.box <- tabBox(width=6, height=400,
                           tabPanel("Table",tags$div(class="cleanTab",HTML(popei3$table))),
                           tabPanel("Sources and Downloads",popei4.info))

       #Append to List
       popei.list <<- list(popei1.box,popei2.box,popei3.box,popei4.box)
       incProgress()
     }  #Employment by Industry

     #Employment and Demographic Forecast
     if("emply" %in% input$outChk){
       #Generate tables, plots and text...
       popem1 <<- jobsPopForecast(fips=substr(fipslist,3,5),ctyname=placeName)
       popem2 <<- weeklyWages(fips=substr(fipslist,3,5),ctyname=placeName)
       popem3 <<- residentialLF(fips=substr(fipslist,3,5),ctyname=placeName)


       #Contents of Information Tabs
       popem1.info <- tags$div(boxContent(title= "Jobs and Population Forecast Plot",
                                          description = "The Jobs and Population Forecast Plot displays the growth rate in local jpbs and population.",
                                          MSA= "T", stats = "F",table = "F",
                                          urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/"),
                                                         c("SDO Jobs Forecasts","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                          tags$br(),
                                          downloadObjUI("popem1plot"),
                                          tags$br(), tags$br(),
                                          downloadObjUI("popem1data"))

       popem2.info <- tags$div(boxContent(title= "Average Weekly wages",
                                          description = "The Average Weekly Wages plot shows the trend in average wages from 2010 to the present for a selected place and the state.",
                                          MSA= "F", stats = "F", table = "F",
                                          urlList = list(c("Department of Labor and Employment Quarterly Census of Employment and Wages","https://www.colmigateway.com/gsipub/index.asp?docid=372") )),
                               tags$br(),
                               downloadObjUI("popem2plot"),
                               tags$br(), tags$br(),
                               downloadObjUI("popem2data"))


       popem3.info <- tags$div(boxContent(title= "Residential Labor Force Participation Line Plot",
                                          description = "The residential Labor Force Line plot shows the trend in total labor gorce participation from 2010 to the present for a selected place and the state.",
                                          MSA= "F", stats = "F",table = "F",
                                          urlList = list(c("SDO Labor Force Participation Data","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                         tags$br(),
                                          downloadObjUI("popem3plot"),
                                          tags$br(), tags$br(),
                                          downloadObjUI("popem3data"))

       popem4.info <- tags$div(boxContent(title= "Residential Labor Force Participation Bar Plot",
                                          description = "The residential Labor Force Bar Plot compares the percentage of persons age 16 and older in the labor force for a selected place to the percentage in the state.",
                                          MSA= "F", stats = "F",table = "F",
                                          urlList = list(c("SDO Labor Force Participation Data","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                               tags$br(),
                               downloadObjUI("popem4plot"),
                               tags$br(), tags$br(),
                               downloadObjUI("popem4data"))


       # Bind to boxes
       popem1.box <- tabBox(width=6, height=400,
                            tabPanel("Plot",renderPlot({popem1$plot},height=340)),
                            tabPanel("Sources and Downloads",popem1.info))
       popem2.box <- tabBox(width=6, height=400,
                            tabPanel("Table",renderPlot({popem2$plot},height=340)),
                            tabPanel("Sources and Downloads",popem2.info))
       popem3.box <- tabBox(width=6, height=400,
                            tabPanel("Plot",renderPlot({popem3$plot1},height=340)),
                            tabPanel("Sources and Downloads",popem3.info))
       popem4.box <- tabBox(width=6, height=400,
                            tabPanel("Plot",renderPlot({popem3$plot2},height=340)),
                            tabPanel("Sources and Downloads",popem4.info))


       #Append to List
       popem.list <<- list(popem1.box,popem2.box,popem3.box,popem4.box)
       incProgress()
     }  #Employment and Demographic Forecast
        }) #Progress Bar
         }#if input$unit == ""

     # Output UI...

    if(length(outputList) == 0) {
    tabs <- lapply(1:length(input$outChk), function(i) {  # this determines the number of tabs needed
      id <- paste0("tab", i)
      tabPanel(
        title = tabTitle(input$outChk[[i]]), tabList(input$outChk[[i]])
      ) # TabPanel
    })
    }  else {
      tabs <- outputList
    }
     output$ui  <- renderUI({ do.call(tabsetPanel, tabs) }) #renderUI
  }) #observeEvent input$profile

  #Event to output PDF documents

 output$singlePDF <- downloadHandler(
    # For PDF output, change this to "report.pdf"
 filename  <- function(){
       paste0(simpleCap(input$unit)," Community Profile ",format(Sys.Date(),"%Y%m%d"), ".pdf")
      },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document

      params <- list(outChk = input$outChk,
                     fips =  listTofips(popPlace("County"),input$level,input$unit),
                     ctyName = simpleCap(input$unit),
                     curACS = curACS,
                     curYr = curYr
                     )



      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
     rmarkdown::render(tempReport, output_file = file,
                       params = params,
                       envir = new.env(parent = globalenv())
     )
   }
  )  # Output singlePDF

   #Event to outload plots and data files
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
   callModule(downloadObj, id = "poph1plot", simpleCap(input$unit),"poph1plot", poph1$plot)
   callModule(downloadObj, id = "poph1data", simpleCap(input$unit),"poph1data", poph1$data)
   callModule(downloadObj, id = "poph2data", simpleCap(input$unit),"poph2data", poph2$data)
   callModule(downloadObj, id = "poph3data", simpleCap(input$unit), "poph3data", poph3$data)
   callModule(downloadObj, id = "poph4data", simpleCap(input$unit), "poph4data", poph4$data)
   callModule(downloadObj, id = "poph5data", simpleCap(input$unit), "poph5data", poph5$data)
   callModule(downloadObj, id = "poph6data", simpleCap(input$unit), "poph6data", poph5$data)

   #commuting
   callModule(downloadObj, id = "popt1plot", simpleCap(input$unit),"popt1plot", popt1$plot)
   callModule(downloadObj, id = "popt2data", simpleCap(input$unit),"popt2data", popt1$data1)
   callModule(downloadObj, id = "popt3data", simpleCap(input$unit),"popt3data", popt1$data2)
   callModule(downloadObj, id = "popt4plot", simpleCap(input$unit),"popt4plot", popt2$plot)
   callModule(downloadObj, id = "popt4data", simpleCap(input$unit),"popt4data", popt2$data)

   #Employment by Industry
   callModule(downloadObj, id = "popei1plot", simpleCap(input$unit),"popei1plot", popei1$plot)
   callModule(downloadObj, id = "popei1data", simpleCap(input$unit),"popei1data", popei1$data)
   callModule(downloadObj, id = "popei2plot", simpleCap(input$unit),"popei2plot", popei2$plot)
   callModule(downloadObj, id = "popei2data", simpleCap(input$unit),"popei2data", popei2$data)
   callModule(downloadObj, id = "popei3plot", simpleCap(input$unit),"popei3plot", popei3$plot)
   callModule(downloadObj, id = "popei3data", simpleCap(input$unit),"popei3data", popei3$data1)
   callModule(downloadObj, id = "popei4data", simpleCap(input$unit),"popei4data", popei3$data2)

   #Employment and Demographic Forecast
   callModule(downloadObj, id = "popem1plot", simpleCap(input$unit),"popem1plot", popem1$plot)
   callModule(downloadObj, id = "popem1data", simpleCap(input$unit),"popem1data", popem1$data)
   callModule(downloadObj, id = "popem2plot", simpleCap(input$unit),"popem2plot", popem2$plot)
   callModule(downloadObj, id = "popem2data", simpleCap(input$unit),"popem2data", popem2$data)
   callModule(downloadObj, id = "popem3plot", simpleCap(input$unit),"popem3plot", popem3$plot1)
   callModule(downloadObj, id = "popem3data", simpleCap(input$unit),"popem3data", popem3$data1)
   callModule(downloadObj, id = "popem4plot", simpleCap(input$unit),"popem4plot", popem3$plot2)
   callModule(downloadObj, id = "popem4data", simpleCap(input$unit),"popem4data", popem3$data2)


  }  #server


shinyApp(ui = ui, server = server)
