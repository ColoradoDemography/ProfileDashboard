#' raceTab2 Table showing the percentage values by ethnic/race categories
#'
#'    pulls data from API This table compares Colorado % to selected geography
#'
#'    This table reports the MOEs and a significance test for each series
#'    comparing the percentages from each table...
#'
#' @param ctyfips is the fips code for the selected county
#' @param ctyname is the name of the selected county
#' @param placefips is the fips code for the selected municipality
#' @param placename is the name of the selected municipality
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param oType output type html table or latex table
#' @return kable formatted  table and data file
#' @export

raceTab2 <- function(listID, ACS, oType) {
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  
  state="08"

  #output race tab using pull from API
  
  #call to ACS County Race variables
  ACSRaceCTY=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", ctyfips, sep=""),meta="no")
  #Converting values to numeric
  ACSRaceCTY[,7:ncol(ACSRaceCTY)]=as.numeric(as.character(ACSRaceCTY[,7:ncol(ACSRaceCTY)]))

  ACSRaceCTY2 <- ACSRaceCTY %>%
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


  f.ACSRaceCTY <- gather(ACSRaceCTY2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)

  ACSRaceCTYMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", "08", ctyfips, sep=""),meta="no")

  ACSRaceCTYMOE[is.na(ACSRaceCTYMOE)] <- 0
  ACSRaceCTYMOE[,7:ncol(ACSRaceCTYMOE)] <- as.numeric(as.character(ACSRaceCTYMOE[,7:ncol(ACSRaceCTYMOE)]))

  ACSRaceCTYMOE2 <- ACSRaceCTYMOE %>%
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

  f.ACSRaceCTYMOE_Fin <- gather(ACSRaceCTYMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  # the county file
  f.county <- merge(f.ACSRaceCTY,f.ACSRaceCTYMOE_Fin,by="race")
  names(f.county) <- c("Race","Count_County","MOE_County")
  
  total_County <- as.numeric(f.county[which(f.county$Race == "TotalPop"),2])
  f.county$CountPCT_County <- f.county$Count_County/total_County
  f.county$MOEPCT_County <- f.county$MOE_County/total_County


  #call to ACS, State Table
  ACSRaceST=codemog_api(data="b03002", db=ACS, geonum=paste("1", state,  sep=""),meta="no")
  #Converting values to numeric
  ACSRaceST[,7:ncol(ACSRaceST)]=as.numeric(as.character(ACSRaceST[,7:ncol(ACSRaceST)]))

  ACSRaceST2 <- ACSRaceST %>%
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

  f.ACSRaceST <- gather(ACSRaceST2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)

  # State level MOEs

  ACSRaceSTMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")
  ACSRaceSTMOE[is.na(ACSRaceSTMOE)] <- 0
  ACSRaceSTMOE[,7:ncol(ACSRaceSTMOE)]=as.numeric(as.character(ACSRaceSTMOE[,7:ncol(ACSRaceSTMOE)]))

  ACSRaceSTMOE2 <- ACSRaceSTMOE %>%
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

  f.ACSRaceSTMOE_Fin <- gather(ACSRaceSTMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  # the state file
  f.state <- merge(f.ACSRaceST,f.ACSRaceSTMOE_Fin,by="race")
  names(f.state) <- c("Race","Count_State","MOE_State")
  
  total_State <- as.numeric(f.state[which(f.state$Race == "TotalPop"),2])
  f.state$CountPCT_State <- f.state$Count_State/total_State
  f.state$MOEPCT_State <- f.state$MOE_State/total_State

if(nchar(placefips) != 0) {
  #call to ACS place Race variables
  ACSRacePL=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", placefips, sep=""),meta="no")
  #Converting values to numeric
  ACSRacePL[,7:ncol(ACSRacePL)]=as.numeric(as.character(ACSRacePL[,7:ncol(ACSRacePL)]))
  
  ACSRacePL2 <- ACSRacePL %>%
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
  
  
  f.ACSRacePL <- gather(ACSRacePL2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  ACSRacePLMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", "08", placefips, sep=""),meta="no")
  
  ACSRacePLMOE[is.na(ACSRacePLMOE)] <- 0
  ACSRacePLMOE[,7:ncol(ACSRacePLMOE)] <- as.numeric(as.character(ACSRacePLMOE[,7:ncol(ACSRacePLMOE)]))
  
  ACSRacePLMOE2 <- ACSRacePLMOE %>%
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
  
  f.ACSRacePLMOE_Fin <- gather(ACSRacePLMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
 
  # the state file
  f.place <- merge(f.ACSRacePL,f.ACSRacePLMOE_Fin,by="race")
  names(f.place) <- c("Race","Count_Place","MOE_Place")
  
  total_Place <- as.numeric(f.place[which(f.place$Race == "TotalPop"),2])
  f.place$CountPCT_Place <- f.place$Count_Place/total_Place
  f.place$MOEPCT_Place <- f.place$MOE_Place/total_Place
}

if(nchar(placefips) == 0){
  f.raceFin <- merge(f.county,f.state, by="Race")
  #Calculating the statistical test
  f.raceFin$ZScore <- (abs(f.raceFin$CountPCT_County - f.raceFin$CountPCT_State)/
                         sqrt((f.raceFin$MOEPCT_County^2) + (f.raceFin$MOEPCT_State^2)))
  f.raceFin$Sig_Diff <- ifelse(f.raceFin$ZScore < 1,"No","Yes")
  f.raceFin$Sig_Diff <- ifelse(is.na(f.raceFin$Sig_Diff)," ",f.raceFin$Sig_Diff)
  
  #Formatting Percentage Values
  f.raceFin$CountPCT_County <- percent(f.raceFin$CountPCT_County*100)
  f.raceFin$MOEPCT_County <- percent(f.raceFin$MOEPCT_County*100)
  f.raceFin$CountPCT_State <- percent(f.raceFin$CountPCT_State*100)
  f.raceFin$MOEPCT_State <- percent(f.raceFin$MOEPCT_State*100)
  
  # table Heading
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 1)
  # set vector names
  names(tblHead) <- c(" ", ctyname,"Colorado"," ")
} else {
  f.raceFin <- merge(f.place,f.county, by="Race")
  #Calculating the statistical test
  f.raceFin$ZScore <- (abs(f.raceFin$CountPCT_Place - f.raceFin$CountPCT_County)/
                         sqrt((f.raceFin$MOEPCT_Place^2) + (f.raceFin$MOEPCT_County^2)))
  f.raceFin$Sig_Diff <- ifelse(f.raceFin$ZScore < 1,"No","Yes")
  f.raceFin$Sig_Diff <- ifelse(is.na(f.raceFin$Sig_Diff)," ",f.raceFin$Sig_Diff)
  
  #Formatting Percentage Values
  f.raceFin$CountPCT_Place <- percent(f.raceFin$CountPCT_Place*100)
  f.raceFin$MOEPCT_Place <- percent(f.raceFin$MOEPCT_Place*100)
  f.raceFin$CountPCT_County <- percent(f.raceFin$CountPCT_County*100)
  f.raceFin$MOEPCT_County <- percent(f.raceFin$MOEPCT_County*100)
  
  # create vector with colspan
  tblHead <- c(" " = 1, placename = 2, ctyname  = 2, " " = 1)
  # set vector names
  names(tblHead) <- c(" ", placename,ctyname," ")
}
  


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




  m.race <- as.matrix(f.raceFin[c(1,9,8,4,3,2,5,6,7,10),c(1,4,5,8,9,11)]) #This is the matrix table

  #Column Names

  names_spaced <- c("Race","Percentage","Margin of Error","Percentage","Margin of Error","Sig. Diff.?")



 if(oType == "html") {
  race_t <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrrr',
          caption="Race Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width ="0.5in") %>%
    column_spec(4, width ="0.5in") %>%
    column_spec(5, width ="0.5in") %>%
    column_spec(6, width ="0.5in") %>%
    add_indent(c(3:9)) %>%
    add_header_above(header=tblHead) %>%
    add_footnote(captionSrc("ACS",ACS))


  race_data <- data.frame(m.race)
  
  if(nchar(placefips) == 0) {
  names(race_data)[1] <- "Race Category"
  names(race_data)[2] <- paste0("Percentage: ",ctyname)
  names(race_data)[3] <- paste0("Margin of Error: ",ctyname)
  names(race_data)[4] <- "Percentage: Colorado"
  names(race_data)[5] <- "Margin of Error: Colorado"
  names(race_data)[6] <- "Signficant Difference?"
  } else {
    names(race_data)[1] <- "Race Category"
    names(race_data)[2] <- paste0("Percentage: ",placename)
    names(race_data)[3] <- paste0("Margin of Error: ",placename)
    names(race_data)[4] <- paste0("Percentage: ",ctyname)
    names(race_data)[5] <- paste0("Margin of Error: ",ctyname)
    names(race_data)[6] <- "Signficant Difference?"  
  }



  outListR <- list("table" = race_t, "data" = race_data)

  return(outListR)
}
  if(oType == "latex") {
    tabOut <- m.race %>% kable(
                    col.names = names_spaced,
                    align=c("l",rep("r",5)),
                    caption="Race Comparison", row.names=FALSE,
                    format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options=c("scale_down","HOLD_position")) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      column_spec(6, width ="0.5in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      add_footnote(captionSrc("ACS",ACS))

    return(tabOut)
  }
}
