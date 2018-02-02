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
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25092 <- codemog_api(data="b25092", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  f.AcsPl <- cbind(f.b25033[,c(1,9:14)], f.b25032[,c(9:19)],f.b25037[,c(1,9)],f.b25010[,c(1,9)],f.b25077[,c(1,8)],f.b25092[,c(1,8)])

  f.AcsPl <- f.AcsPl[,c(1:18,20,22,24, 26)]

  f.AcsPl[,2:22] <-as.numeric(as.character(f.AcsPl[,2:22]))


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
    PPH = b25010002,
    Med_Val = b25077001,
    PCT_INC = b25092001)


  f.AcsPlL <- f.AcsPl[,c(1,23:38)] %>% gather(var, ACS, People_TOT:PCT_INC, -geoname)

  # Raw Place MOE
  f.b25033_moe <- codemog_api(data="b25033_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Population by housing type
  f.b25032_moe <- codemog_api(data="b25032_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Units in Structure
  f.b25037_moe <- codemog_api(data="b25037_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Year Built
  f.b25010_moe <- codemog_api(data="b25010_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Persons per Household
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25092_moe <- codemog_api(data="b25092_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  f.AcsPl_moe <- cbind(f.b25033_moe[,c(1,9:14)], f.b25032_moe[,c(9:19)],f.b25037_moe[,c(1,9)],f.b25010_moe[,c(1,9)],f.b25077_moe[,c(1,8)],f.b25092_moe[,c(1,8)])

  f.AcsPl_moe <- f.AcsPl_moe[,c(1:18,20,22,24, 26)]

  f.AcsPl_moe[,2:22] <-as.numeric(as.character(f.AcsPl_moe[,2:22]))


  f.AcsPl_moe <- f.AcsPl_moe %>% mutate(
    People_TOT = b25033_moe002,
    People_1 = b25033_moe003,
    People_2_4 = b25033_moe004,
    People_5 =  b25033_moe005,
    People_MH = b25033_moe006,
    People_OTH = b25033_moe007,
    Units_TOT = b25032_moe002,
    Units_1 = sqrt(b25032_moe003^2 + b25032_moe004^2),
    Units_2_4 = sqrt(b25032_moe005^2 + b25032_moe006^2),
    Units_5 = sqrt(b25032_moe007^2 + b25032_moe008^2 + b25032_moe009^2 + b25032_moe010^2),
    Units_MH = b25032_moe011,
    Units_OTH = b25032_moe012,
    Med_Yr = b25037_moe002,
    PPH = b25010_moe002,
    Med_Val = b25077_moe001,
    PCT_INC = b25092_moe001)

  f.AcsPlL_moe <- f.AcsPl_moe[,c(1,23:38)] %>% gather(var, MOE, People_TOT:PCT_INC, -geoname)

  f.AcsPl_Fin <- merge(f.AcsPlL,f.AcsPlL_moe,by ="var")

  f.AcsPl_Fin <- f.AcsPl_Fin[c(9,4:8,16,11:15,1,2,10,3),c(1:3,5)]
  names(f.AcsPl_Fin) <- c("var","Place","Pl_VAL", "Pl_MOE")

  #calculating proportions

  # Splitting File
  #People
  PlPval <- f.AcsPl_Fin[c(1:6),]

  Ptot <- as.numeric(PlPval[1,3])

  PlPval$Pl_VAL_P <- as.numeric(PlPval$Pl_VAL)/as.numeric(Ptot)
  PlPval$Pl_MOE_P <- as.numeric(PlPval$Pl_MOE)/as.numeric(Ptot)

  #units
  PlUval <- f.AcsPl_Fin[c(7:12),]
  Utot <- as.numeric(PlUval[1,3])

  PlUval$Pl_VAL_P <- as.numeric(PlUval$Pl_VAL)/as.numeric(Utot)
  PlUval$Pl_MOE_P <- as.numeric(PlUval$Pl_MOE)/as.numeric(Utot)

  # Remainder
  PlRval <- f.AcsPl_Fin[c(13:16),]
  PlRval$Pl_VAL_P <- NA
  PlRval$Pl_MOE_P <- NA

  # reassembling fils
  f.AcsPl_Fin <- rbind(PlPval,PlUval,PlRval)

  # Assembling State Data

  # Raw State data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state),meta="no") # Persons per Household
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25092 <- codemog_api(data="b25092", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  f.AcsSt <- cbind(f.b25033[,c(1,9:14)], f.b25032[,c(9:19)],f.b25037[,c(1,9)],f.b25010[,c(1,9)],f.b25077[,c(1,8)],f.b25092[,c(1,8)])

  f.AcsSt <- f.AcsSt[,c(1:18,20,22,24, 26)]

  f.AcsSt[,2:22] <-as.numeric(as.character(f.AcsSt[,2:22]))


  f.AcsSt <- f.AcsSt %>% mutate(
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
    PPH = b25010002,
    Med_Val = b25077001,
    PCT_INC = b25092001)

  f.AcsStL <- f.AcsSt[,c(1,23:38)] %>% gather(var, ACS, People_TOT:PCT_INC, -geoname)

  # Raw Place MOE
  f.b25033_moe <- codemog_api(data="b25033_moe", db=ACS, geonum=paste0("1", state),meta="no") # Population by housing type
  f.b25032_moe <- codemog_api(data="b25032_moe", db=ACS, geonum=paste0("1", state),meta="no") # Units in Structure
  f.b25037_moe <- codemog_api(data="b25037_moe", db=ACS, geonum=paste0("1", state),meta="no") # Year Built
  f.b25010_moe <- codemog_api(data="b25010_moe", db=ACS, geonum=paste0("1", state),meta="no") # Persons per Household
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25092_moe <- codemog_api(data="b25092_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  f.AcsSt_moe <- cbind(f.b25033_moe[,c(1,9:14)], f.b25032_moe[,c(9:19)],f.b25037_moe[,c(1,9)],f.b25010_moe[,c(1,9)],f.b25077_moe[,c(1,8)],f.b25092_moe[,c(1,8)])

  f.AcsSt_moe <- f.AcsSt_moe[,c(1:18,20,22,24, 26)]

  f.AcsSt_moe[,2:22] <-as.numeric(as.character(f.AcsSt_moe[,2:22]))


  f.AcsSt_moe <- f.AcsSt_moe %>% mutate(
    People_TOT = b25033_moe002,
    People_1 = b25033_moe003,
    People_2_4 = b25033_moe004,
    People_5 =  b25033_moe005,
    People_MH = b25033_moe006,
    People_OTH = b25033_moe007,
    Units_TOT = b25032_moe002,
    Units_1 = sqrt(b25032_moe003^2 + b25032_moe004^2),
    Units_2_4 = sqrt(b25032_moe005^2 + b25032_moe006^2),
    Units_5 = sqrt(b25032_moe007^2 + b25032_moe008^2 + b25032_moe009^2 + b25032_moe010^2),
    Units_MH = b25032_moe011,
    Units_OTH = b25032_moe012,
    Med_Yr = b25037_moe002,
    PPH = b25010_moe002,
    Med_Val = b25077_moe001,
    PCT_INC = b25092_moe001)

  f.AcsStL_moe <- f.AcsSt_moe[,c(1,23:38)] %>% gather(var, MOE, People_TOT:PCT_INC, -geoname)

  f.AcsSt_Fin <- merge(f.AcsStL,f.AcsStL_moe,by ="var")

  f.AcsSt_Fin <- f.AcsSt_Fin[c(9,4:8,16,11:15,1,2,10,3),c(1:3,5)]
  names(f.AcsSt_Fin) <- c("var","State","St_VAL", "St_MOE")

  #calculating proportions

  # Splitting File
  #People
  StPval <- f.AcsSt_Fin[c(1:6),]

  Ptot <- as.numeric(StPval[1,3])

  StPval$St_VAL_P <- as.numeric(StPval$St_VAL)/as.numeric(Ptot)
  StPval$St_MOE_P <- as.numeric(StPval$St_MOE)/as.numeric(Ptot)

  #units
  StUval <- f.AcsSt_Fin[c(7:12),]
  Utot <- as.numeric(StUval[1,3])

  StUval$St_VAL_P <- as.numeric(StUval$St_VAL)/as.numeric(Utot)
  StUval$St_MOE_P <- as.numeric(StUval$St_MOE)/as.numeric(Utot)

  # Remainder
  StRval <- f.AcsSt_Fin[c(13:16),]
  StRval$St_VAL_P <- NA
  StRval$St_MOE_P <- NA

  # reassembling fils
  f.AcsSt_Fin <- rbind(StPval,StUval,StRval)

  # Joining Fles
  f.OOHouse <- merge(f.AcsPl_Fin,f.AcsSt_Fin, by="var")

  #calculating statistical test
  f.OOHouse$ZScore <- ifelse(is.na(f.OOHouse$Pl_VAL_P), (abs(f.OOHouse$Pl_VAL - f.OOHouse$St_VAL)/sqrt((f.OOHouse$Pl_MOE^2) + (f.OOHouse$St_MOE^2))),
                             (abs(f.OOHouse$Pl_VAL_P - f.OOHouse$St_VAL_P)/sqrt((f.OOHouse$Pl_MOE_P^2) + (f.OOHouse$St_MOE_P^2))))
  f.OOHouse$SigDif <- ifelse(f.OOHouse$ZScore < 1, "No","Yes")

  # in table Order, formats and selecting output columns
  f.OOHouse <- f.OOHouse[c(16,11:15,9,4:8,1,2,10,3),]

  f.OOHouse$Pl_VAL_F <- ifelse(f.OOHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=0, big.mark=",")),
                               ifelse(f.OOHouse$var == "PPH", formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=2),
                                      ifelse(f.OOHouse$var == "Med_Yr", formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=0),
                                             ifelse(f.OOHouse$var == "PCT_INC",percent(f.OOHouse$Pl_VAL),formatC(as.numeric(f.OOHouse$Pl_VAL), format="f", digits=0, big.mark=",")
                                             ))))
  f.OOHouse$Pl_MOE_F <- ifelse(f.OOHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.OOHouse$Pl_MOE), format="f", digits=0, big.mark=",")),
                               ifelse(f.OOHouse$var == "PPH", formatC(as.numeric(f.OOHouse$Pl_MOE), format="f", digits=2),
                                      ifelse(f.OOHouse$var == "Med_Yr", formatC(as.numeric(f.OOHouse$Pl_MOE), format="f", digits=0),
                                             ifelse(f.OOHouse$var == "PCT_INC",percent(f.OOHouse$Pl_MOE),formatC(as.numeric(f.OOHouse$Pl_MOE), format="f", digits=0, big.mark=",")
                                             ))))
  f.OOHouse$St_VAL_F <- ifelse(f.OOHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.OOHouse$St_VAL), format="f", digits=0, big.mark=",")),
                               ifelse(f.OOHouse$var == "PPH", formatC(as.numeric(f.OOHouse$St_VAL), format="f", digits=2),
                                      ifelse(f.OOHouse$var == "Med_Yr", formatC(as.numeric(f.OOHouse$St_VAL), format="f", digits=0),
                                             ifelse(f.OOHouse$var == "PCT_INC",percent(f.OOHouse$St_VAL),formatC(as.numeric(f.OOHouse$St_VAL), format="f", digits=0, big.mark=",")
                                             ))))
  f.OOHouse$St_MOE_F <- ifelse(f.OOHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.OOHouse$St_MOE), format="f", digits=0, big.mark=",")),
                               ifelse(f.OOHouse$var == "PPH", formatC(as.numeric(f.OOHouse$St_MOE), format="f", digits=2),
                                      ifelse(f.OOHouse$var == "Med_Yr", formatC(as.numeric(f.OOHouse$St_MOE), format="f", digits=0),
                                             ifelse(f.OOHouse$var == "PCT_INC",percent(f.OOHouse$St_MOE),formatC(as.numeric(f.OOHouse$St_MOE), format="f", digits=0, big.mark=",")
                                             ))))

  f.OOHouse$Pl_VAL_PF <- percent(f.OOHouse$Pl_VAL_P*100)
  f.OOHouse$Pl_MOE_PF <- percent(f.OOHouse$Pl_MOE_P*100)
  f.OOHouse$St_VAL_PF <- percent(f.OOHouse$St_VAL_P*100)
  f.OOHouse$St_MOE_PF <- percent(f.OOHouse$St_MOE_P*100)

  f.OOHouse$Pl_VAL_PF <- ifelse(is.na(f.OOHouse$Pl_VAL_P),f.OOHouse$Pl_VAL_F,f.OOHouse$Pl_VAL_PF)
  f.OOHouse$Pl_MOE_PF <- ifelse(is.na(f.OOHouse$Pl_MOE_P),f.OOHouse$Pl_MOE_F,f.OOHouse$Pl_MOE_PF)
  f.OOHouse$St_VAL_PF <- ifelse(is.na(f.OOHouse$St_VAL_P),f.OOHouse$St_VAL_F,f.OOHouse$St_VAL_PF)
  f.OOHouse$St_MOE_PF <- ifelse(is.na(f.OOHouse$St_MOE_P),f.OOHouse$St_MOE_F,f.OOHouse$St_MOE_PF)

  f.OOHouse_Fin <- f.OOHouse[,c(1,14,15,18,19,16,17,20,21,13)]
  f.OOHouse_Fin[c(1,7),c(4,5,8,9,10)] <- ""
  f.OOHouse_Fin[c(13:16),c(4,5,8,9)] <- ""

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
                                                                                                                  ifelse(f.OOHouse$var =="PPH","Average Number of Persons Per Household",
                                                                                                                         ifelse(f.OOHouse$var =="Med_Val","Median Value (Current $)","Median Costs as a Percentage of Income"
                                                                                                                         )))))))))))))))


  names(f.OOHouse_Fin)  <-c("Variable",paste0("Value: ",ctyname),paste0("Margin of Error: ", ctyname),
                            paste0("Percentage Value: ",ctyname), paste0("Percentage Margin of Error: ", ctyname),
                            "Value: Colorado","Margin of Error: Colorado",
                            "Percentage Value: Colorado", "Percentage Margin of Error: Colorado", "Signifcant Difference?")


  m.OOHouse <- as.matrix(f.OOHouse_Fin)

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Count","Margin of Error","Percent","Margin of Error","Count","Margin of Error","Percent","Margin of Error","Significant Difference?")
  #Span Header

  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 4, "Colorado" = 4, " " = 1)

  # set vector names
  names(tblHead1) <- c(" ", ctyname, "Colorado", " ")

  if(oType == "html") {
  Housing_tab <- m.OOHouse %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrrrrrrrrr',
          caption="Characteristics of Owner-Occupied Housing",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 10) %>%
    row_spec(0, align = "c") %>%
    row_spec(1, bold = TRUE, italic = TRUE) %>%
    row_spec(7, bold = TRUE, italic = TRUE) %>%
    row_spec(13, bold = TRUE, italic = TRUE) %>%
    column_spec(1, width = "80em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    column_spec(7, width ="5em") %>%
    column_spec(8, width ="5em") %>%
    column_spec(9, width ="5em") %>%
    column_spec(10, width ="5em") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))

  outList <- list("table" = Housing_tab, "data" = f.OOHouse_Fin)
  return(outList)
  }

  if(oType == "latex") {

  tabOut <-  kable(m.OOHouse,
    col.names = names_spaced,
    align=c("l",rep("r",9)),
    caption="Characteristics of Owner-Occupied Housing", row.names=FALSE,
    format="latex", booktabs=FALSE)  %>%
    kable_styling(latex_options = "scale_down") %>%
    row_spec(0, align = "c") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))

  return(tabOut)
  }
}
