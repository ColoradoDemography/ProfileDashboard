#' raceTab2 Table showing the percentage values by ethnic/race categories
#'
#'    pulls data from API This table compares Colorado % to selected geography
#'
#'    This table reports the MOEs and a significance test for each series
#'    comparing the percentages from each table...
#'
#' @param fips the short FIPS code
#' @param ctyname Place Name
#' @param ACS data depository from the American Community Survey API
#' @param oType Type of output, html or latex
#' @return kable formatted table and data file
#' @export

raceTab2 <- function(fips,ctyname,ACS, oType) {

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

  ACSRaceMOE[is.na(ACSRaceMOE)] <- 0
  ACSRaceMOE[,7:ncol(ACSRaceMOE)] <- as.numeric(as.character(ACSRaceMOE[,7:ncol(ACSRaceMOE)]))

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

  names_spaced <- c("Race","Percentage","Margin of Error","Percentage","Margin of Error","Sig. Diff.?")

  #Span Header

  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 1)

  # set vector names
  names(tblHead) <- c(" ", ctyname,"Colorado"," ")
 if(oType == "html") {
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
    column_spec(1, width = "30em") %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    add_indent(c(3:9)) %>%
    add_header_above(header=tblHead) %>%
    add_footnote(captionSrc("ACS",ACS))


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
  if(oType == "latex") {
    tabOut <- m.race %>% kable(
                    col.names = names_spaced,
                    align=c("l",rep("r",5)),
                    caption="Race Comparison", row.names=FALSE,
                    format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options=c("scale_down","HOLD_position")) %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      add_footnote(captionSrc("ACS",ACS))

    return(tabOut)
  }
}
