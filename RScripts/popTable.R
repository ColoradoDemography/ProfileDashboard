#' popTable The population table showing the annual growth rate in the Population Section
#'
#' @param cty county FIPS code, without the state code
#' @param ctyname the county name
#' @param place place fips code, without the state code
#' @param placename the place name
#' @param sYr Start Year
#' @param eYr End year
#' @param oType Output Type, html or latex
#' @return kable formatted  table and data file
#' @export
#'
popTable <- function(cty,ctyname,place,placename,sYr,eYr,oType) {
  #outputs the Population Growth Rate table in the population section..

  placename <- as.character(unique(placename))
  state <- "Colorado"
  ctynum <- as.numeric(cty)
  placenum <- as.numeric(place)
  yrs <- as.character(setYrRange(sYr,eYr))
  
  #State Population and Growth Rate
  popCO=county_profile(0, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=2),
           Population=comma(totalpopulation))
  mCO <- as.matrix(popCO[,c(1,5,7,6)])
  
  #County Population and Growth Rate  *** need to account for multip county communities...
  mCty <- matrix(nrow=7,ncol=1)
  
  for(i in 1:length(ctynum)) {
  x=county_profile(ctynum[i], sYr:eYr, "totalpopulation")%>%
     filter(year %in% yrs)%>%
     arrange(county,year)%>%
     mutate(name=county,
            year=as.numeric(year),
            totalpopulation=as.numeric(totalpopulation),
            growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=2),
            Population=comma(totalpopulation))
     mCty <- cbind(mCty, as.matrix(x[,c(3,5,7,6)]))
  }
  

  
  if(nchar(placename) != 0) { #if a placename is present
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",placenum,";")
    # Postgres Call to gather municipal jobs numbers
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
    
    f.popPlace <-  dbGetQuery(con, sqlStrPop1)
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)
 
     f.popPlace <- f.popPlace[which(f.popPlace$countyfips != 999), ]  # removing "Total" for multi-county cities
     PP <-  f.popPlace %>% group_by(placefips, municipalityname, year)  %>% summarize(totalpopulation = sum(as.numeric(totalpopulation)))
     
     placX <- PP %>% 
      filter(year %in% yrs)%>%
      arrange(year)
     
     placX$Population <- format(placX$totalpopulation,big.mark=",")
     placX$growthRate  <- percent((((placX$totalpopulation/lag(placX$totalpopulation))^(1/(placX$year-lag(placX$year)))) -1)*100,digits=2)
     mPlace <- as.matrix(placX[,c(3,2,5,6)])
  }
 

  if(nchar(placename) != 0) { #if a placename is present
    m.OutTab <- cbind(mPlace,mCty,mCO)
      m.OutTab <- m.OutTab[,c(1,3,4,8,9,12,13)]
    }  else {
    m.OutTab <- cbind(mCty,mCO)
    m.OutTab <- m.OutTab[,c(2,4,5,8,9)] 
    } 

  m.OutTab <- gsub("NA%","",m.OutTab)

  if(nchar(placename) != 0) {
        names_spaced <- c("Year","Population","Annual Growth Rate","Population","Annual Growth Rate","Population","Annual Growth Rate") 
        tblHead <- c(" " = 1, placename = 2, ctyname = 2, state = 2)
        names(tblHead) <- c(" ", placename, ctyname,state)
   } else {
    names_spaced <- c("Year","Population","Annual Growth Rate","Population","Annual Growth Rate")
    tblHead <- c(" " = 1, ctyname = 2, state = 2)
    names(tblHead) <- c(" ", ctyname,state)
  }
  
  


 

  if(oType == "html") {
    # Creating Final Table (kable)
    if(nchar(placename) != 0) {
            OutTab  <- m.OutTab %>%
              kable(format='html', table.attr='class="myTable"',
                    caption = "Population Growth Rate",
                    row.names=FALSE,
                    align='lrrrrrr',
                    col.names = names_spaced,
                    escape = FALSE) %>%
              kable_styling(bootstrap_options = "condensed") %>%
              column_spec(1, width = "0.4in") %>%
              column_spec(2, width = "0.5in") %>%
              column_spec(3, width ="0.5in") %>%
              column_spec(4, width = "0.5in") %>%
              column_spec(5, width = "0.5in") %>%
              column_spec(6, width = "0.5in") %>%
              column_spec(7, width = "0.5in") %>%
              add_header_above(header=tblHead)  %>%
              add_footnote(captionSrc("SDO",""))
    }  else { 
      OutTab  <- m.OutTab %>%
        kable(format='html', table.attr='class="myTable"',
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed") %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }
    
    # Creating Final Data Set
    f.Out2 <- as.data.frame(m.OutTab)
    if(ncol(f.Out2) == 5) {
      names(f.Out2) <- c("Year",paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
    }
    if(ncol(f.Out2) == 7) {
      names(f.Out2) <- c("Year",paste0("Population: ",placename),paste0("Growth Rate: ",placename),
                         paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
    }
    if(ncol(f.Out2) == 9) {
      names(f.Out2) <- c("Year",paste0("Population: ",placename),paste0("Growth Rate: ",placename),
                         paste0("Population: ",ctyname[1]),paste0("Growth Rate: ",ctyname[1]),
                         paste0("Population: ",ctyname[2]),paste0("Growth Rate: ",ctyname[2]),
                         "Population: Colorado","Growth Rate: Colorado")
    }
    

    # bind list
    outList <- list("table" = OutTab,"data" = f.Out2)

    return(outList)
  }


  if(oType == "latex") {
    if(nchar(placename) != 0) {
        OutTab  <- m.OutTab %>%
          kable(format ="latex", booktabs=TRUE,
                caption = "Population Growth Rate",
                row.names=FALSE,
                align='lrrrrrr',
                col.names = names_spaced,
                escape = FALSE) %>%
          kable_styling(bootstrap_options = "condensed", font_size=10.5) %>%
          column_spec(1, width = "0.4in") %>%
          column_spec(2, width = "0.5in") %>%
          column_spec(3, width = "0.5in") %>%
          column_spec(4, width = "0.5in") %>%
          column_spec(5, width = "0.5in") %>%
          column_spec(6, width = "0.5in") %>%
          column_spec(7, width = "0.5in") %>%
          add_header_above(header=tblHead)  %>%
          add_footnote(captionSrc("SDO",""))
     }  else { 
      OutTab  <- m.OutTab %>%
        kable(format ="latex", booktabs=TRUE,
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed", font_size=10.5) %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }

  outList <- list("table" = outKable)
  return(outList)
  }

}
