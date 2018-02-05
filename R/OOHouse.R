#' OOHouse Summary table for owner-occupied Housing
#'
#'
#' @param fips The FIPS of the Place or County to use for the graph
#' @param ctyname The place Name
#' @param ACS  The American Community Survey Vintage
#' @param oType Output Type, html or latex
#' @param state defaults to Colorado
#' @return kable formatted  table and data file
#' @export
#'

OOHouse=function(fips, ctyname, ACS, oType, state="08"){

  # Raw Place data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, fips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, fips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, fips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, fips),meta="no") # Persons per Household

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


  m.OOHouse <- as.matrix(f.OOHouse_Fin)

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Value","Percent")
  #Span Header

  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 2)

  # set vector names
  names(tblHead1) <- c(" ", ctyname)

  if(oType == "html") {
  Housing_tab <- m.OOHouse %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption="Characteristics of Owner-Occupied Housing",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 10) %>%
    row_spec(0, align = "c") %>%
    row_spec(1, bold = TRUE, italic = TRUE) %>%
    row_spec(7, bold = TRUE, italic = TRUE) %>%
    row_spec(13, bold = TRUE, italic = TRUE) %>%
    row_spec(14, bold = TRUE, italic = TRUE) %>%
    column_spec(1, width = "3in",bold = T) %>%
    column_spec(2, width = "0.4in") %>%
    column_spec(3, width ="0.4in") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))

  outList <- list("table" = Housing_tab, "data" = f.OOHouse_Fin)
  return(outList)
  }

  if(oType == "latex") {

  tabOut <-  kable(m.OOHouse,
    col.names = names_spaced,
    align="lrr",
    caption="Characteristics of Owner-Occupied Housing", row.names=FALSE,
    format="latex", booktabs=TRUE)  %>%
    kable_styling() %>%
    row_spec(0, align = "c") %>%
    row_spec(1, bold = TRUE, italic = TRUE) %>%
    row_spec(7, bold = TRUE, italic = TRUE) %>%
    row_spec(13, bold = TRUE, italic = TRUE) %>%
    row_spec(14, bold = TRUE, italic = TRUE) %>%
    column_spec(1, width = "5in",bold = T) %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width ="0.5in") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))

  return(tabOut)
  }
}
