#' Community Profile Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, November 2017-March 2018
#' V 1.0 County-level version sent  for testing

rm(list = ls())
library(tidyverse, quietly=TRUE)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(codemogLib)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL, quietly=TRUE)
library(rmarkdown)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(VennDiagram)
library(rgdal)
library(gridExtra)




# The GLOBAL Variables  Add Additional lists items as sections get defined
# Current ACS database
curACS <- "acs1216"
curYr <- 2016
fipslist <<- ""

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
popem4 <<- list()
popem.list <<- list()


# Structure of user Interface
ui <-
  dashboardPage( skin="green", title= "State Demography Office Community Profile",
                 dashboardHeader(title = span(img(src="ShieldOnly_LRG.png", height = 70, align = "top"),"State Demography Office Community Profile"), titleWidth=550),  #dashboardHeader
                 dashboardSidebar( width = 300,  useShinyjs(),
                                   # data level Drop down
                                   selectInput("level", "Select Data Level" ,
                                               choices=c("Select a Data Level","Counties","Municipalities")  #Enabled in V1
                                   ),

                                   # profile Unit dropdown
                                   selectInput("unit", "Select Profile" ,choices=""),
                                   # Comparison dropdown 1  Disabled in V1
                                   #   selectizeInput("comp", "Select Comparison" ,choices=""),
                                   #   # Comparison dropdown 2
                                   #    selectizeInput("comp2","Select Custom Comparisons",choices ="", multiple=TRUE),  Disabled in V1
                                   #Output Content Checkboxes
                                   checkboxGroupInput("outChk", "Select the data elements to display:",
                                                      choices = c("Basic Statistics" = "stats",
                                                                  "Population Trends" = "popf",
                                                                  "Population Characteristics: Age" = "pop",
                                                                  "Population Characteristics: Income, Education and Race"= "popc",
                                                                  "Housing and Households" = "housing",
                                                                  "Commuting and Job Growth" = "comm",
                                                                  "Employment by Industry"="emplind",
                                                                  "Employment Forecast and Wage Information"="emply"
                                                      ),
                                                      selected =  c("stats","popf","pop","popc",
                                                                    "housing","comm", "emplind","emply")
                                   ),

                                   #Action Button
                                   actionButton("profile","View Profile"),
                                   #   actionButton("comparison","View Comparison"),  Disabled in V1
                                   actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')"),
                                   downloadButton("outputPDF", label="Download PDF Report",
                                                  style="color: black; background-color: gray90; border-color: black")
     
                                   
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
                fluidRow(uiOutput("ui")
                         )
                
                 ) #dashboardBody

                 ) # dashboardPage/ui


# Server Management Function
server <- function(input, output, session) {
  shinyjs::hide("outputPDF")
  outputtxt <- tags$div(tags$b("Welcome to the State Demography Office (SDO) Community Profile Dashboard"),
                         tags$br(),
                         tags$p("This tool provides summary plots and data describing counties and municipalities in Colorado."),
                         tags$p("To create a profile:"),
                         tags$ul(
                           tags$li("Select a location using the dropdown boxes."),
                           tags$li("Select specific information to display using the checkboxes."),
                           tags$li("Click on the 'View Profile' button to display the selected profile.")
                         ),
                         tags$p("You can download the plots and underlying data for each display by selecting the 'Sources and Downloads' 
                                panel of each display box."),
                         tags$br(),
                         tags$em(tags$b("Notes:")), 
                          tags$ul(
                            tags$li("Profiles are produced for Municipalites with more than 200 persons.  Please contact SDO for further information."),
                            tags$li("Producing the requested outputs may take up to 3 minutes, depending on your request and your connection speed."),
                           tags$li("Downloading any report, plot or data object will open a new browser window while the object is being processed and downloaded.  This window will close once the object processing is completed."),
                           tags$li("Downloaded objects will be saved in the 'Download' location supported by your browser.")
                           )
                        
                      
  )
  output$ui <- renderUI(outputtxt)
  # updates Dropdown boxes and selects data level and unit
  CountyList <- popPlace("Counties")
  PlaceList <- popPlace("Municipalities")

  CustomList <- list()
  observeEvent(input$level, ({
    shinyjs::hide("outputPDF")
    #clears the comp2 dropdown on change
    updateSelectInput(session, "comp2", choices = "")
    if(input$level == "Select a Data Level") { #the initial state of the dropdowns
      outUnit = ""
      outComp = ""
    }
    #   if(input$level == "Planning Regions") {  Disabled in V1
    #         outUnit <- seq(from=1, to=14, by=1)
    #         outComp <- c("Selected Region Only" ,"State")
    #      }
    if(input$level == "Counties") {
      outUnit <- unique(as.list(CountyList[,3]))
      outComp <- c("Selected County Only", "Counties in Planning Region", "Custom List of Counties (Select Below)","State")
    }
    if(input$level == "Municipalities") {  
              outUnit <- unique(as.list(PlaceList[,3]))
              outComp <- c("Selected Municipality Only", "Similar Municipalities", "County", "Custom List of Municipalities (Select Below)", "State")
                                                  }

    updateSelectInput(session, "unit", choices = outUnit)
    #    updateSelectInput(session, "comp", choices = outComp)
  }))  #observeEvent input$level

  # Event for Comparison selection
  observeEvent(input$comp, {
    shinyjs::hide("outputPDF")
    if((input$level == "Counties") && (input$comp == "Custom List of Counties (Select Below)")){
      # Creating custom list
      custList <- as.list(CountyList[which(CountyList$municipalityname != input$unit),3])
      updateSelectInput(session, "comp2", choices = custList)
    }
    # Disabled in V1
    #                if((input$level == "Municipalities") && (input$comp == "Custom List of Municipalities (Select Below)")){
    #                  # Creating custom list
    #                  custList <- as.list(unique(PlaceList[which(PlaceList$municipalityname != input$unit),3]))
    #                  updateSelectInput(session, "comp2", choices = custList)
    #                }
  }) #observeEvent input$comp

  # Event for click on profile button
  observeEvent(input$profile,  {
    shinyjs::hide("outputPDF")
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
          fipslist <<- listTofips(CountyList,input$level,input$unit)
        } else {  #This is for all other levels, need to write aggergation functions for regions and states
          fipslist <<- listTofips(PlaceList,input$level,input$unit)
         
         }


        #Generate profile UI objects
        
        svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
        placeName <- simpleCap(input$unit)
        ln1 <- tags$h1(placeName)
        #creating ids and output flags for multiple counties and small places
        idList <- chkID(lvl=input$level,fipslist= fipslist,plName=placeName,ctyList=CountyList, plList=PlaceList)

        #stats; Basic Statistics
        if("stats" %in% input$outChk) {
          stats.text <- tags$h2("Basic Statistics")
          if(input$level == "Counties") {
            stats.tab1 <- statsTable1(listID=idList,sYr=2010,eYr=2016,ACS=curACS,oType="html")
         #   stats.map <- dashboardMAP(listID=idList,placelist="")
          }
          if(input$level == "Municipalities") {
            stats.tab1 <- statsTable1(listID=idList,sYr=2010,eYr=2016,ACS=curACS,oType="html")
         #   stats.map <- dashboardMAP(listID=idList,placelist=PlaceList)
          }
          

          Stats.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabl in each display box.",tags$br(),
                                 "Note: County data is displayed for municiaplities and places with fewer than 200 people.",tags$br(), tags$br(),
                                 "General information is available here:", tags$br(),
                                 tags$ul(
                                   tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                   tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank")
                                   )))

          stats.box0 <- box(width=12,ln1)
          stats.box1 <- tabBox(width=12, height=350,
                               tabPanel("Table",tags$div(class="Row1Tab",HTML(stats.tab1))),
                               tabPanel("Information",Stats.info))
     #     stats.box1 <- tabBox(width=8, height=350,
     #                          tabPanel("Table",tags$div(class="Row1Tab",HTML(stats.tab1))),
     #                          tabPanel("Information",Stats.info))
     #     stats.box2 <- box(width=4, height=350,renderPlot({stats.map},height=270))


          #building List
     #     stats.list <<- list(stats.box0, stats.box1, stats.box2)
          stats.list <<- list(stats.box0, stats.box1)
          incProgress()
        }
        # Population Forecasts
        
       
        if("popf" %in% input$outChk){
          #Chart/Table Objects
          popf1 <<- popTable(listID=idList,sYr=1990,eYr=curYr,oType="html")
          popf2 <<- pop_timeseries(listID=idList,endyear=curYr,base=12)
          popf3 <<- popForecast(listID=idList)
          popf4 <<- cocPlot(listID=idList,lyr=curYr)

          #infobox Objects
          if(input$level == "Counties") {
          popf1.info <- tags$div(boxContent(title= "Population Growth Estimates",
                                            description = "The Population Growth Table compares population growth for a place to the State.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                 tags$br(),
                                 downloadObjUI("popf1data"))

          popf2.info <- tags$div(boxContent(title= "Population Growth Data",
                                            description = "The Population Growth Plot shows the growth of the total population for a selected location.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                 tags$br(), downloadObjUI("popf2plot"),  downloadObjUI("popf2data"))

          popf3.info <- tags$div(boxContent(title= "Population Forecast",
                                            description = "The Population Forecast plot shows the estimated population growth between 2010 and 2025 for the selected county.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO Population Totals for Colorado Counties","https://demography.dola.colorado.gov/population/population-totals-counties/#population-totals-for-colorado-counties")) ),
                                 tags$br(), downloadObjUI("popf3plot"), downloadObjUI("popf3data"))

          popf4.info <- tags$div(boxContent(title= "Components of Change",
                                            description = "The Components of Change Plot shows the estimated births, deaths and net migration values for a selected place between 2010 and the present.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList=list(c("SDO Components of Change Estimates","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/"))),
                                 tags$br(), downloadObjUI("popf4plot"), downloadObjUI("popf4data"))
          }
          
          if(input$level == "Municipalities") {
            popf1.info <- tags$div(boxContent(title= "Population Growth Estimates",
                                              description = "The Population Growth Table compares population growth for a place to the State.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popf1data"))
            
            popf2.info <- tags$div(boxContent(title= "Population Growth Data",
                                              description = "The Population Growth Plot shows the growth of the total population for a selected location.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(), downloadObjUI("popf2plot"),  downloadObjUI("popf2data"))
            
            popf3.info <- tags$div(boxContent(title= "Population Forecast",
                                              description = "The Population Forecast plot shows the estimated population growth between 2010 and 2025 for the selected county.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("SDO Population Totals for Colorado Counties","https://demography.dola.colorado.gov/population/population-totals-counties/#population-totals-for-colorado-counties")) ),
                                   tags$br(), downloadObjUI("popf3plot"), downloadObjUI("popf3data"))
            
            popf4.info <- tags$div(boxContent(title= "Components of Change",
                                              description = "The Components of Change Plot shows the estimated births, deaths and net migration values for a selected place between 2010 and the present.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList=list(c("SDO Components of Change Estimates","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/"))),
                                   tags$br(), downloadObjUI("popf4plot"), downloadObjUI("popf4data"))
          }
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
 
            popa1 <<- agePlotPRO(listID=idList, ACS=curACS, yrs=curYr)
            popa2 <<- medianAgeTab(listID=idList, ACS=curACS,oType="html")
            popa3 <<- ageForecastPRO(listID=idList,sYr=2010,mYr=2015,eYr=2025,base=12)
            popa4 <<- migbyagePRO(listID=idList)

          #Info Boxes
          if(input$level == "Counties") {   
            popa1.info <- tags$div(boxContent(title= "Population by Age",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/"),
                                                             c("SDO Age Visualization Chart","https://demography.dola.colorado.gov/Age-Animation-Bars/")) ),
                                   tags$br(),
                                   downloadObjUI("popa1plot"), downloadObjUI("popa1data"))
  
          popa2.info <- tags$div(boxContent(title= "Median Age Data",
                                            description = "The Median Age Table compares the median age by gender for a location to the state.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("American Community Survey American Fact Finder, Series B01002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popa2data"))

          popa3.info <- tags$div(boxContent(title= "Population Forecast by Age",
                                            description = "The Population Forecast by Age Plot displays the age distribution between 2010 and 2025.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                 tags$br(),
                                 downloadObjUI("popa3plot"), downloadObjUI("popa3data"))

          popa4.info <- tags$div(boxContent(title= "Net Migration by Age",
                                            description = "The Net Migration by Age Plot compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/")) ),
                                 tags$br(),
                                 downloadObjUI("popa4plot"), downloadObjUI("popa4data"))
          }

          if(input$level == "Municipalities") {          
            popa1.info <- tags$div(boxContent(title= "Population by Age",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("American Community Survey American Fact Finder, Series B01001","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                   tags$br(),
                                   downloadObjUI("popa1plot"), downloadObjUI("popa1data"))
            
            popa2.info <- tags$div(boxContent(title= "Median Age Data",
                                              description = "The Median Age table compares the median age by gender for a location to the state.",
                                              MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                              urlList = list(c("American Community Survey American Fact Finder, Series B01002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                   tags$br(),
                                   downloadObjUI("popa2data"))
            
            popa3.info <- tags$div(boxContent(title= "Population Forecast by Age",
                                              description = "The Population Forecast by Age Plot displays the age distribution between 2010 and 2025 .",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popa3plot"), downloadObjUI("popa3data"))
            
            popa4.info <- tags$div(boxContent(title= "Net Migration by Age",
                                              description = "The Net Migration by Age Plot compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                              urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/")) ),
                                   tags$br(),
                                   downloadObjUI("popa4plot"), downloadObjUI("popa4data"))
          }
          
          # Bind to boxes
          popa1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot", renderPlot({popa1$plot},height=340)),
                              tabPanel("Sources and Downloads",popa1.info))
          popa2.box <- tabBox(width=6, height=400,
                              tabPanel("Table", tags$div(class="cleanTab", HTML(popa2$table))),
                              tabPanel("Sources and Downloads",popa2.info))
          popa3.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderPlot({popa3$plot})),
                              tabPanel("Sources and Downloads", popa3.info))
          popa4.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderPlot({popa4$plot})),
                              tabPanel("Sources and Downloads",popa4.info))


          #Append to List
          popa.list <<- list(popa1.box,popa2.box,popa3.box,popa4.box)
          incProgress()
        }


        # Population Chatacteristics
        if("popc" %in% input$outChk){
          #Generate tables, plots and text...
            popc1 <<- incomePRO(listID=idList, ACS=curACS)
            popc2 <<- educPRO(listID=idList, ACS=curACS)
            popc3 <<- raceTab1(listID=idList, ACS=curACS,oType="html")
            popc4 <<- raceTab2(listID=idList, ACS=curACS,oType="html")
          
         
          
          #Contents of Information Tabs
          popc1.info <- tags$div(boxContent(title= "Household Income",
                                            description = "The Household Income Disctibution Plot compares the distribution of household income for a selected location to the state.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                          c("American Community Survey American Fact Finder, Series B19001","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc1plot"),  downloadObjUI("popc1data"))

          popc2.info <- tags$div(boxContent(title= "Education Attainment",
                                            description= "The Educational Attainment Plot compares the categories of educational attaiment for adults aged 25 and older for a selected location to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, Series B15003","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc2plot"),  downloadObjUI("popc2data"))


          popc3.info <- tags$div(boxContent(title= "Racial Identification Trend",
                                            description= "The Race Trend Table shows changes in the distribution of racial idenification since the 2000 Census.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, Series B03002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc3data"))

          popc4.info <- tags$div(boxContent(title= "Racial Identification Comparison",
                                            description= "The Race Comparison Table compares the distribution of racial idenification of a place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, series B03002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
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
              poph1 <<- houseEstPRO(listID=idList,curYr=curYr)
              poph2 <<- housePRO(listID=idList, ACS=curACS,oType="html")
              poph3 <<- OOHouse(listID=idList,ACS=curACS,oType="html")
              poph4 <<- RTHouse(listID=idList,ACS=curACS,oType="html")
              poph5 <<- HouseVal(listID=idList,ACS=curACS,oType="html")
 

          #Contents of Information Tabs
          poph1.info <- tags$div(boxContent(title= "Household Projection",
                                            description = "The household projection displays the estimated number of households between 2010 and 2050.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO Household Projections --County","https://demography.dola.colorado.gov/housing-and-households/data/household-projections/")) ),
                                 tags$br(),
                                 downloadObjUI("poph1plot"),  downloadObjUI("poph1data"))

          poph2.info <- tags$div(boxContent(title= "Housing Type Table",
                                            description= "The Housing Type Table compares the categories of housing types for a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("SDO Housing Time Series","https://demography.dola.colorado.gov/population/data/muni-pop-housing/"),
                                                           c("American Community Survey American Fact Finder, Series B25001, B25003, and B25004","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph2data"))


          poph3.info <- tags$div(boxContent(title= "Characteristics of Owner-Occupied Housing",
                                            description= "The Owner-Occupied Housing Table displays the characteristics of owner-occupied housing in a selected place.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25010, B25032, B25033, and B25037","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph3data"))

          poph4.info <- tags$div(boxContent(title= "Characteristics of Rental Housing",
                                            description= "The Rental Housing Table displays the characteristics of rental housing in a selected place.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25010, B25032, B25033, and B25037","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph4data"))
          poph5.info <- tags$div(boxContent(title= "Comparative Owner-Occupied Housing Values",
                                            description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25077 and B25092","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph5data"))
          
          poph6.info <- tags$div(boxContent(title= "Comparative Rental Housing Values",
                                            description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25066 and B25071","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph6data"))


          # Bind to boxes
          poph1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderPlot({poph1$plot},height=340)),
                              tabPanel("Sources and Downloads",poph1.info))
          poph2.box <- tabBox(width=6, height=400,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(poph2$table))),
                              tabPanel("Sources and Downloads",poph2.info))
          poph5.box <- tabBox(width=6, height = 325,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(poph5$OOTab))),
                              tabPanel("Sources and Downloads",poph5.info))
          poph6.box <- tabBox(width=6, height = 325,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(poph5$RTTab))),
                              tabPanel("Sources and Downloads",poph6.info))
          poph3.box <- tabBox(width=6, height=350,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(poph3$table))),
                              tabPanel("Sources and Downloads",poph3.info))
          poph4.box <- tabBox(width=6, height=350,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(poph4$table))),
                              tabPanel("Sources and Downloads",poph4.info))


          #Append to List
          poph.list <<- list(poph1.box,poph2.box, poph5.box, poph6.box, poph3.box,poph4.box)
          incProgress()
        }

        # Commuting
        if("comm" %in% input$outChk){

          #Generate tables, plots and text...

          popt1 <<- GenerateVenn(listID=idList,oType="html")
          popt2 <<- jobMigration(listID=idList,maxyr = curYr)

          #Contents of Information Tabs
          popt1.info <- tags$div(boxContent(title= "Commuting Patterns Plot",
                                            description = "The Communting Patterns plot shows the number of people working and living in a specified location.",
                                            MSA= "T", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt1plot"))

          popt2.info <- tags$div(boxContent(title= "Work Outside Table",
                                            description= "The work outside table shows the top ten work locations for prople living in an area but working somewhere else.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt2data"))


          popt3.info <- tags$div(boxContent(title= "Live Outside Table",
                                            description= "The live outside table shows the top ten residential locations for people working in an area but living somewhere else.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt3data"))

          popt4.info <- tags$div(boxContent(title= "Jobs and Net Migration Plot",
                                            description= "The jobs and net migration plot shows the trend between jobs and net migration for a selected place.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                            urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/"),
                                                           c("Bureau of Economic Analysis Jobs Data","https://www.bea.gov/index.htm"))),
                                 tags$br(),
                                 downloadObjUI("popt4plot"), downloadObjUI("popt4data"))

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
          popt.list <<- list(popt1.box,popt4.box,popt2.box,popt3.box)
          incProgress()
        }  #Commuting

        #Employment by Industry
        if("emplind" %in% input$outChk){
          #Generate tables, plots and text...
          popei1 <<- jobsPlot(listID=idList, maxyr = curYr)
          popei2 <<- jobsByIndustry(listID=idList, curyr = curYr)
          popei3 <<- baseIndustries(listID=idList, curyr = curYr, oType="html")

          #Contents of Information Tabs
          popei1.info <- tags$div(boxContent(title= "Estimated Jobs",
                                             description = "The Jobs Estimate Plot shows the estimated number of jobs to 2040.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("Jobs by Sector (NAICS)","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                  tags$br(),
                                  downloadObjUI("popei1plot"),  downloadObjUI("popei1data"))

          popei2.info <- tags$div(boxContent(title= "Jobs by Sector / Economic Industry Mix",
                                             description= "Comparing the share of jobs by industry to a larger area helps to get a better understanding of the industries that higher or lower employment concentrations.  The industry mix can also help inform the average weekly wages as industries such as retail trade or 
                                             accommodation and food pay considerably lower wages than professional and technical services or mining.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("Jobs by Sector (NAICS)","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                  tags$br(),
                                  downloadObjUI("popei2plot"), downloadObjUI("popei2data"))


          popei3.info <- tags$div(boxContent(title= "Base Industries Plot",
                                             description= "The Base Industries plot shows which industries drive the county economy by bringing in dollars from outside the area.  A county with a diversity of base industries with similar shares of employment will 
                                                 generally be more resilient than one that is dominated by one large industry.",
                                             MSA= "T", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("SDO Base industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                  tags$br(),
                                  downloadObjUI("popei3plot"), downloadObjUI("popei3data"))

          popei4.info <- tags$div(boxContent(title= "Base Industries Table",
                                             description= "The Base Industries Table summarizes the number of jobs in indirect basic employment, direct basic employment and local services sectors.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
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
          popem1 <<- jobsPopForecast(listID=idList,curyr=curYr)
          popem2 <<- weeklyWages(listID=idList)
          popem3 <<- residentialLF(listID=idList,curyr=curYr)
          popem4 <<- incomeSrc(level=input$level,listID=idList,ACS=curACS, oType="html")  


          #Contents of Information Tabs
          popem1.info <- tags$div(boxContent(title= "Jobs and Population Forecast Plot",
                                             description = "The Jobs and Population Forecast Plot displays the growth rate in local jpbs and population.",
                                             MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("SDO Economic Forecasts"," https://demography.dola.colorado.gov/economy-labor-force/economic-forecasts/#economic-forecasts"),
                                                            c("SDO Jobs Forecasts","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                  tags$br(),
                                  downloadObjUI("popem1plot"), downloadObjUI("popem1data"))

          popem2.info <- tags$div(boxContent(title= "Average Weekly wages",
                                             description = "The Average Weekly Wages plot shows the trend in average wages from 2010 to the present for a selected place and the state.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("Department of Labor and Employment Quarterly Census of Employment and Wages","https://www.colmigateway.com/gsipub/index.asp?docid=372") )),
                                  tags$br(),
                                  downloadObjUI("popem2plot"),  downloadObjUI("popem2data"))


          popem3.info <- tags$div(boxContent(title= "Residential Labor Force Participation Line Plot",
                                             description = "The Residential Labor Force Line plot shows the trend in total labor gorce participation from 2010 to the present for a selected place and the state.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "F",
                                             urlList = list(c("SDO Labor Force Participation Data","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                  tags$br(),
                                  downloadObjUI("popem3plot"), downloadObjUI("popem3data"))

          popem4.info <- tags$div(boxContent(title= "Household Income Sources(s) Table",
                                             description = "The Houselold Income Source(s) Table shows household income sources and amounts for housholds in a selected place or county.  
                                             Households will have multiple sources of income, so this table is not mutually exclusive. Mean income values reflect values from the cited source.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, table = "T",
                                             urlList = list(c("American Community Survey American Fact Finder, Series B19051 to B19070","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                  tags$br(),
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
                               tabPanel("Table",tags$div(class="cleanTab",HTML(popem4$table))),
                               tabPanel("Sources and Downloads",popem4.info))


          #Append to List
          popem.list <<- list(popem1.box,popem2.box,popem3.box,popem4.box)
          incProgress()
        }  #Employment and Demographic Forecast
        
        #Generate Report
        tempReport <- "SDO_Report.Rnw"
        tempTex <- "SDO_Report.tex"
        incProgress()
        
        # Set up parameters to pass to Rnw document
        outChk <- input$outChk
        olistID <- idList
        olevel <- input$level
        ocurACS <- curACS
        ocurYr <- curYr
        placelist <- PlaceList
        incProgress()
        
        #knitting file and copy to final document
        knit(input=tempReport,output=tempTex)
        incProgress() 
        tools::texi2pdf(tempTex)
        incProgress()       
       shinyjs::show("outputPDF") 
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
    
    #Event to output PDF documents
    
    output$outputPDF <- downloadHandler(
        filename <- function() {
             paste0(input$unit," Community Profile Report ",as.character(Sys.Date()),".pdf")
          },
        content <- function(file) {
          tempPDF <- "SDO_Report.pdf"
          file.rename(tempPDF, file) # move pdf to file for downloading
        } #Content
    ) #Download Handler

        
  
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
    callModule(downloadObj, id = "popem4data", simpleCap(input$unit),"popem4data", popem4$data)
    
  }) #observeEvent input$profile




}  #server



shinyApp(ui = ui, server = server)
