#' OOHouse Summary table for owner-occupied Housing
#'
#'
#' @param ctyfips is the fips code for the selected county
#' @param ctyname is the name of the selected county
#' @param placefips is the fips code for the selected municipality
#' @param placename is the name of the selected municipality
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param oType output type html table or latex table
#' @return kable formatted  table and data file
#' @export
#'

OOHouse=function(ctyfips,ctyname, placefips, placename, ACS, oType, state="08"){
if(nchar(placefips) == 0) {
  # Raw Place data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Persons per Household
} else {
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Persons per Household
  
}
  f.AcsPl <- cbind(f.b25033[,c(1,9:14)], f.b25032[,c(9:19)],f.b25037[,c(1,9)],f.b25010[,c(1,9)])

  f.AcsPl <- f.AcsPl[,c(1:18,20,22)]

  f.AcsPl[,2:20] <-as.numeric(as.character(f.AcsPl[,2:20]))


  f.AcsPl <- f.AcsPl %>% mutate(
    People_TOT = b25033002,
    People_1 = b25033003,
    People_2_4 = b25033004,
    People_5 =  b25033005,
    People_MH = b25033006,
    People_OTH = b25033007,
    Units_TOT = b25032002,
    Units_1 = b25032003 + b25032004,
    Units_2_4 = b25032005 + b25032006,
    Units_5 = b25032007 + b25032008 + b25032009 + b25032010,
    Units_MH = b25032011,
    Units_OTH = b25032012,
    Med_Yr = b25037002,
    PPH = b25010002)


  f.AcsPlL <- f.AcsPl[,c(1,21:34)] %>% gather(var, ACS, People_TOT:PPH, -geoname)

  f.AcsPl_Fin <- f.AcsPlL

  f.AcsPl_Fin <- f.AcsPl_Fin[,c(2,1,3)]
  names(f.AcsPl_Fin) <- c("var","Place","Pl_VAL")

  #calculating proportions

  # Splitting File
  #People
  PlPval <- f.AcsPl_Fin[c(1:6),]

  Ptot <- as.numeric(PlPval[1,3])

  PlPval$Pl_VAL_P <- as.numeric(PlPval$Pl_VAL)/as.numeric(Ptot)


  #units
  PlUval <- f.AcsPl_Fin[c(7:12),]
  Utot <- as.numeric(PlUval[1,3])

  PlUval$Pl_VAL_P <- as.numeric(PlUval$Pl_VAL)/as.numeric(Utot)


  # Remainder
  PlRval <- f.AcsPl_Fin[c(13,14),]
  PlRval$Pl_VAL_P <- NA


  # reassembling fils
  f.AcsPl_Fin <- rbind(PlPval,PlUval,PlRval)


  # Joining Fles
  f.OOHouse <- f.AcsPl_Fin

  f.OOHouse$Pl_VAL_F <-  ifelse(f.OOHouse$var == "PPH", formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=2),
                                ifelse(f.OOHouse$var == "Med_Yr", formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=0), formatC(as.numeric(f.OOHouse$Pl_VAL),format="f", digits=0, big.mark=",")))


  f.OOHouse$Pl_VAL_PF <- percent(f.OOHouse$Pl_VAL_P*100)

  f.OOHouse$Pl_VAL_PF <- ifelse(is.na(f.OOHouse$Pl_VAL_P),f.OOHouse$Pl_VAL_F,f.OOHouse$Pl_VAL_PF)

  f.OOHouse_Fin <- f.OOHouse[,c(1,5,6)]
  f.OOHouse_Fin[c(13,14),3] <- ""

  #Renaming rows and Columns
  f.OOHouse_Fin$var <- ifelse(f.OOHouse$var =="People_TOT", "Total Number of People in Owner-Occupied Housing",
                              ifelse(f.OOHouse$var =="People_1","People Living in Single Unit Buildings",
                                     ifelse(f.OOHouse$var =="People_2_4","People Living in Buildings with 2 to 4 Units",
                                            ifelse(f.OOHouse$var =="People_5","People Living in Buildings with 5 or More Units",
                                                   ifelse(f.OOHouse$var =="People_MH","People Living in Mobile Homes",
                                                          ifelse(f.OOHouse$var =="People_OTH","People Living in RVs, Boats, Vans, Etc.",
                                                                 ifelse(f.OOHouse$var =="Units_TOT","Total Number of Owner-Occupied Housing Units",
                                                                        ifelse(f.OOHouse$var =="Units_1","Units per Building: 1",
                                                                               ifelse(f.OOHouse$var =="Units_2_4","Units per Building 2 to 4",
                                                                                      ifelse(f.OOHouse$var =="Units_5","Units per Building: 5 or More",
                                                                                             ifelse(f.OOHouse$var =="Units_MH","Number of Mobile Homes",
                                                                                                    ifelse(f.OOHouse$var =="Units_OTH","Number of RVs, Boats, Vans, Etc.",
                                                                                                           ifelse(f.OOHouse$var =="Med_Yr","Median Year of Construction",
                                                                                                                  ifelse(f.OOHouse$var =="PPH","Average Number of Persons Per Household",""
                                                                                                                  ))))))))))))))


  names(f.OOHouse_Fin)  <-c("Variable",paste0("Value: ",ctyname),
                            paste0("Percentage Value: ",ctyname))

  #Building table
  m.OOHouse <- matrix(nrow=8,ncol=5,"")

  m.OOHouse[1:6,2:3] <- as.matrix(f.OOHouse_Fin[1:6,2:3])  #People
  m.OOHouse[1:6,4:5] <- as.matrix(f.OOHouse_Fin[7:12,2:3]) #UNits
  m.OOHouse[7,4] <- f.OOHouse_Fin[13,2]
  m.OOHouse[8,2] <- f.OOHouse_Fin[14,2]

  m.OOHouse[1,1] <- "Owner-Occupied Housing"
  m.OOHouse[2,1] <- "Single Unit Buildings"
  m.OOHouse[3,1] <- "Buildings with 2 to 4 Units"
  m.OOHouse[4,1] <- " Buildings with 5 or More Units"
  m.OOHouse[5,1] <- "Mobile Homes"
  m.OOHouse[6,1] <- "RVs, Boats, Vans, Etc."
  m.OOHouse[7,1] <- "Median Year of Construction"
  m.OOHouse[8,1] <- "Average Number of Persons Per Household"

  m.OOHouse[6,4] <- format(as.numeric(m.OOHouse[6,4]),big.mark=",")

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Value","Percent","Value","Percent")
  #Span Header
if(nchar(placefips) == 0) {
  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 4)

  # set vector names
  names(tblHead1) <- c(" ", ctyname)
} else {
  # create vector with colspan
  tblHead1 <- c(" " = 1, placename = 4)
  
  # set vector names
  names(tblHead1) <- c(" ", placename)
}

  tblHead2 <- c(" " = 1, "People" = 2, "Units" = 2)
  names(tblHead2) <- c(" ","People","Units")

  if(oType == "html") {
    Housing_tab <- m.OOHouse %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrr',
            caption="Characteristics of Owner-Occupied Housing",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      add_indent(c(2:6)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    outList <- list("table" = Housing_tab, "data" = f.OOHouse_Fin)
    return(outList)
  }

  if(oType == "latex") {

    tabOut <-  kable(m.OOHouse,
                     col.names = names_spaced,
                     align="lrrrr",
                     caption="Characteristics of Owner-Occupied Housing", row.names=FALSE,
                     format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      add_indent(c(2:6)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    return(tabOut)
  }
}
