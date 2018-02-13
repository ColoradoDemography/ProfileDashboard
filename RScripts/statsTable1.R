#' statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
#'
#' @param cty the County  FIPS code, including the state value
#' @param place the Place FIPS Code, including the state value.
#' @param sYr Start Year
#' @param eYr End year
#' @param ACS American Cummunity Survey Data series
#' @param oType Controls the rendering of the table, HTML or Latex
#' @return kable formatted table
#' @export
#'
statsTable1 <- function(cty,place,sYr,eYr,ACS,oType){
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
    plName <- paste0(tPopyr1$county," County")
    tJobs <-  county_jobs(fips=as.numeric(ctyfips), year = eYr) #County
    hhinc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    MedHHValue <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    # Poverty Value
    Poverty <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctPoverty <- percent((as.numeric(Poverty$b17001002)/as.numeric(Poverty$b17001001))*100)
    # Percent native
    Native <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctNative <- percent((as.numeric(Native$b05002003)/as.numeric(Native$b05002001))*100)
    #Cost of Living Index  Removed in V1.
#    coli=county_coli%>%
#      filter(countyfips==as.numeric(ctyfips))%>%
#      mutate(coli_level=paste(coli, level, sep=", "))%>%
#     select(coli_level)
  }
  if(nchar(place) > 0) {  #Places
    tPopyr1 <- muni_est(as.numeric(placefips), sYr,as.numeric(ctyfips),"totalpopulation")
    tPopyr2 <- muni_est(as.numeric(placefips), eYr,as.numeric(ctyfips),"totalpopulation")
    plName <- tPopyr1$municipality
    tJobs <-  county_jobs(fips=as.numeric(ctyfips), year = eYr)#County
    hhinc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    MedHHValue <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    #muni Coli

      Poverty <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      pctPoverty <- percent(as.numeric(Poverty$b17001002)/as.numeric(Poverty$b17001001))

      Native <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      pctNative <- percent(as.numeric(Native$b05002003)/as.numeric(Native$b05002001))

    #Cost of Living Index removed in V1
#    coli=county_coli%>%
#      filter(countyfips==as.numeric(ctyfips))%>%
#      mutate(coli_level=paste(coli, level, sep=", "))%>%
#      select(coli_level)
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

# outTab[3,2] <- coli$coli_level
# outTab[4,2] <- "County Cost of Living Index (CO=100)*"
  outTab[3,2] <- ""
  outTab[4,2] <- ""
  outTab[5,1] <- paste0("$",format(as.numeric(hhinc$b19013001),nsmall=0, big.mark=","))
  outTab[6,1] <- paste0("Median Income (Colorado: ","$",format(as.numeric(hhinc_state$b19013001),nsmall=0, big.mark=","),")+")

  outTab[5,2] <- paste0("$",format(as.numeric(MedHHValue$b25077001),nsmall=0, big.mark=","))
  outTab[6,2] <- paste0("Median House Value (Colorado: ","$",format(as.numeric(MedHHValue_state$b25077001),nsmall=0, big.mark=","),")+")

  outTab[7,1] <- pctPoverty
  outTab[7,2] <- pctNative

  outTab[8,1] <- "Percentage of Population with Incomes lower than the Poverty Line+"
  outTab[8,2] <- "Percentage of Population Born in Colorado+"

  outTab[9,1] <- paste0("*",captionSrc("SDO",""))
  outTab[9,2] <- paste0("+",captionSrc("ACS",ACS))

  if(oType == "html") {
    outKable <- kable(outTab, format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='cc',
          caption="Community Quick Facts",
          escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
      column_spec(1, width = "30em") %>%
      column_spec(2, width = "30em")
  }
  if(oType == "latex") {
    latTab <- matrix(nrow=11, ncol=3,"")
    latTab[1,2] <- plName
    latTab[1,3] <- "Colorado"
    latTab[2,1] <- outTab[2,1]
    latTab[2,2] <- outTab[1,1]
    latTab[3,1] <- outTab[2,2]
    latTab[3,2] <- outTab[1,2]
    latTab[4,1] <- outTab[4,1]
    latTab[4,2] <- outTab[3,1]
    latTab[5,1] <- outTab[4,2]
    latTab[5,2] <- outTab[3,2]
    latTab[6,1] <- outTab[8,1]
    latTab[6,2] <- outTab[7,1]
    latTab[7,1] <- outTab[8,2]
    latTab[7,2] <- outTab[7,2]
    latTab[8,1] <- "Median Income+"
    latTab[8,2] <- outTab[5,1]
    latTab[8,3] <- paste0("$",format(as.numeric(hhinc_state$b19013001),nsmall=0, big.mark=","))
    latTab[9,1] <- "Median House Value+"
    latTab[9,2] <- outTab[5,2]
    latTab[9,3] <- paste0("$",format(as.numeric(MedHHValue_state$b25077001),nsmall=0, big.mark=","))
    latTab[10,1] <- outTab[9,1]
    latTab[11,1] <- outTab[9,2]

    outKable <- kable(latTab, caption="Community Quick Facts",align="lrr",
                      format ="latex", booktabs=TRUE) %>%
                kable_styling(latex_options="HOLD_position") %>%
                row_spec(9, font_size=9) %>%
                row_spec(10, font_size=9) %>%
                column_spec(1, width="4in") %>%
                column_spec(2, width="1in") %>%
                column_spec(3, width="1in")

  }
  return(outKable)
}
