#' RTHouse Summary table for Rental Housing
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

RTHouse=function(ctyfips,ctyname, placefips, placename, ACS, oType, state="08"){
if(nchar(placefips) == 0) {
  # Raw Place data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Persons per Household
}  else {
  # Raw Place data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Persons per Household
  
  }


  f.AcsPl <- cbind(f.b25033[,c(1,15:20)], f.b25032[,c(20:30)],f.b25037[,c(1,10)],f.b25010[,c(1,10)])

  f.AcsPl <- f.AcsPl[,c(1:18,20,22)]

  f.AcsPl[,2:20] <-as.numeric(as.character(f.AcsPl[,2:20]))


  f.AcsPl <- f.AcsPl %>% mutate(
    People_TOT = b25033008,
    People_1 = b25033009,
    People_2_4 = b25033010,
    People_5 = b25033011,
    People_MH = b25033012,
    People_OTH = b25033013,
    Units_TOT = b25032013,
    Units_1 = b25032014 + b25032015,
    Units_2_4 = b25032016 + b25032017,
    Units_5 = b25032018 + b25032019 + b25032020 + b25032021,
    Units_MH = b25032022,
    Units_OTH = b25032023,
    Med_Yr = b25037003,
    PPH = b25010003)

  f.AcsPlL <- f.AcsPl[,c(1,21:34)] %>% gather(var, ACS, People_TOT:PPH, -geoname)
  names(f.AcsPlL) <- c("geoname","var", "Pl_VAL")


  #calculating proportions

  # Splitting File
  #People
  PlPval <- f.AcsPlL[c(1:6),]

  Ptot <- as.numeric(PlPval[1,3])

  PlPval$Pl_VAL_P <- as.numeric(PlPval$Pl_VAL)/as.numeric(Ptot)

  #units
  PlUval <- f.AcsPlL[c(7:12),]
  Utot <- as.numeric(PlUval[1,3])

  PlUval$Pl_VAL_P <- as.numeric(PlUval$Pl_VAL)/as.numeric(Utot)


  # Remainder
  PlRval <- f.AcsPlL[c(13,14),]
  PlRval$Pl_VAL_P <- NA


  # reassembling fils
  f.RTHouse <- rbind(PlPval,PlUval,PlRval)


  f.RTHouse$Pl_VAL_F <-  ifelse(f.RTHouse$var == "PPH", formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=2),
                                ifelse(f.RTHouse$var == "Med_Yr", formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=0),
                                       ifelse(f.RTHouse$var == "PCT_INC",percent(f.RTHouse$Pl_VAL),formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=0, big.mark=",")
                                       )))

  f.RTHouse$Pl_VAL_PF <- percent(f.RTHouse$Pl_VAL_P*100)

  f.RTHouse$Pl_VAL_PF <- ifelse(is.na(f.RTHouse$Pl_VAL_P),"",f.RTHouse$Pl_VAL_PF)


  #Renaming rows and Columns
  f.RTHouse$var <- ifelse(f.RTHouse$var =="People_TOT", "Total Number of People in Rental Housing",
                          ifelse(f.RTHouse$var =="People_1","People Living in Single Unit Buildings",
                                 ifelse(f.RTHouse$var =="People_2_4","People Living in Buildings with 2 to 4 Units",
                                        ifelse(f.RTHouse$var =="People_5","People Living in Buildings with 5 or More Units",
                                               ifelse(f.RTHouse$var =="People_MH","People Living in Mobile Homes",
                                                      ifelse(f.RTHouse$var =="People_OTH","People Living in RVs, Boats, Vans, Etc.",
                                                             ifelse(f.RTHouse$var =="Units_TOT","Total Number of Rental Housing Units",
                                                                    ifelse(f.RTHouse$var =="Units_1","Units per Building: 1",
                                                                           ifelse(f.RTHouse$var =="Units_2_4","Units per Building: 2 to 4",
                                                                                  ifelse(f.RTHouse$var =="Units_5","Units per Building: 5 or More",
                                                                                         ifelse(f.RTHouse$var =="Units_MH","Number of  Mobile Homes",
                                                                                                ifelse(f.RTHouse$var =="Units_OTH","Number of RVs, Boats, Vans, Etc.",
                                                                                                       ifelse(f.RTHouse$var =="Med_Yr","Median Year of Construction","Average Number of Persons Per Household"
                                                                                                       )))))))))))))
  f.RTHouse <- f.RTHouse[,c(2,5,6)]
  names(f.RTHouse)  <-c("Variable",paste0("Value: ",ctyname), paste0("Percentage Value: ",ctyname))


  #Building table
  m.RTHouse <- matrix(nrow=8,ncol=5,"")

  m.RTHouse[1:6,2:3] <- as.matrix(f.RTHouse[1:6,2:3])  #People
  m.RTHouse[1:6,4:5] <- as.matrix(f.RTHouse[7:12,2:3]) #UNits
  m.RTHouse[7,4] <- f.RTHouse[13,2]
  m.RTHouse[8,2] <- f.RTHouse[14,2]

  m.RTHouse[1,1] <- "Rental Housing"
  m.RTHouse[2,1] <- "Single Unit Buildings"
  m.RTHouse[3,1] <- "Buildings with 2 to 4 Units"
  m.RTHouse[4,1] <- " Buildings with 5 or More Units"
  m.RTHouse[5,1] <- "Mobile Homes"
  m.RTHouse[6,1] <- "RVs, Boats, Vans, Etc."
  m.RTHouse[7,1] <- "Median Year of Construction"
  m.RTHouse[8,1] <- "Average Number of Persons Per Household"

  m.RTHouse[6,4] <- format(as.numeric(m.RTHouse[6,4]),big.mark=",")

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
    Housing_tab <- m.RTHouse %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrr',
            caption="Characteristics of Rental Housing",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      add_indent(c(2:6)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))
    outList <- list("table" = Housing_tab, "data" = f.RTHouse)
    return(outList)
  }

  if(oType == "latex") {
    tabOut <-  kable(m.RTHouse,
                     col.names = names_spaced,
                     align=c("lrrrr"),
                     caption="Characteristics of Rental Housing", row.names=FALSE,
                     format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      add_indent(c(2:6)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    return(tabOut)
  }
}
