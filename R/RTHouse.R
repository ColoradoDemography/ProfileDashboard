#' RTHouse Summary table for Rental Housing
#'
#' @param fips The FIPS of the Place or County to use for the graph
#' @param ctyname The place Name
#' @param ACS  The American Community Survey Vintage
#' @param oType Output Type html or latex
#' @param state defaults to Colorado
#' @return kable formatted table and data file
#' @export
#'

RTHouse=function(fips, ctyname, ACS, oType, state="08"){

  # Raw Place data
  f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, fips),meta="no") # Population by housing type
  f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, fips),meta="no") # Units in Structure
  f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, fips),meta="no") # Year Built
  f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, fips),meta="no") # Persons per Household
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25071 <- codemog_api(data="b25071", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  f.AcsPl <- cbind(f.b25033[,c(1,15:20)], f.b25032[,c(20:30)],f.b25037[,c(1,10)],f.b25010[,c(1,10)],f.b25064[,c(1,8)],f.b25071[,c(1,8)])

  f.AcsPl <- f.AcsPl[,c(1:18,20,22,24, 26)]

  f.AcsPl[,2:22] <-as.numeric(as.character(f.AcsPl[,2:22]))


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
    PPH = b25010003,
    Med_Val = b25064001,
    PCT_INC = b25071001)


  f.AcsPlL <- f.AcsPl[,c(1,23:38)] %>% gather(var, ACS, People_TOT:PCT_INC, -geoname)

  # Raw Place MOE
  f.b25033_moe <- codemog_api(data="b25033_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Population by housing type
  f.b25032_moe <- codemog_api(data="b25032_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Units in Structure
  f.b25037_moe <- codemog_api(data="b25037_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Year Built
  f.b25010_moe <- codemog_api(data="b25010_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Persons per Household
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25071_moe <- codemog_api(data="b25071_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  f.AcsPl_moe <- cbind(f.b25033_moe[,c(1,15:20)], f.b25032_moe[,c(20:30)],f.b25037_moe[,c(1,10)],f.b25010_moe[,c(1,10)],f.b25064_moe[,c(1,8)],f.b25071_moe[,c(1,8)])

  f.AcsPl_moe <- f.AcsPl_moe[,c(1:18,20,22,24, 26)]

  f.AcsPl_moe[,2:22] <-as.numeric(as.character(f.AcsPl_moe[,2:22]))


  f.AcsPl_moe <- f.AcsPl_moe %>% mutate(
    People_TOT = b25033_moe008,
    People_1 = b25033_moe009,
    People_2_4 = b25033_moe010,
    People_5 = b25033_moe011,
    People_MH = b25033_moe012,
    People_OTH = b25033_moe013,
    Units_TOT = b25032_moe013,
    Units_1 =  sqrt(b25032_moe014^2 + b25032_moe015^2),
    Units_2_4 =  sqrt(b25032_moe016^2 + b25032_moe017^2),
    Units_5 = sqrt(b25032_moe018^2 + b25032_moe019 ^2 + b25032_moe020^2 + b25032_moe021^2),
    Units_MH = b25032_moe022,
    Units_OTH = b25032_moe023,
    Med_Yr = b25037_moe003,
    PPH = b25010_moe003,
    Med_Val = b25064_moe001,
    PCT_INC = b25071_moe001)

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
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25071 <- codemog_api(data="b25071", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  f.AcsSt <- cbind(f.b25033[,c(1,15:20)], f.b25032[,c(20:30)],f.b25037[,c(1,10)],f.b25010[,c(1,10)],f.b25064[,c(1,8)],f.b25071[,c(1,8)])

  f.AcsSt <- f.AcsSt[,c(1:18,20,22,24, 26)]

  f.AcsSt[,2:22] <-as.numeric(as.character(f.AcsSt[,2:22]))


  f.AcsSt <- f.AcsSt %>% mutate(
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
    PPH = b25010003,
    Med_Val = b25064001,
    PCT_INC = b25071001)

  f.AcsStL <- f.AcsSt[,c(1,23:38)] %>% gather(var, ACS, People_TOT:PCT_INC, -geoname)

  # Raw Place MOE
  f.b25033_moe <- codemog_api(data="b25033_moe", db=ACS, geonum=paste0("1", state),meta="no") # Population by housing type
  f.b25032_moe <- codemog_api(data="b25032_moe", db=ACS, geonum=paste0("1", state),meta="no") # Units in Structure
  f.b25037_moe <- codemog_api(data="b25037_moe", db=ACS, geonum=paste0("1", state),meta="no") # Year Built
  f.b25010_moe <- codemog_api(data="b25010_moe", db=ACS, geonum=paste0("1", state),meta="no") # Persons per Household
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25071_moe <- codemog_api(data="b25071_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  f.AcsSt_moe <- cbind(f.b25033_moe[,c(1,15:20)], f.b25032_moe[,c(20:30)],f.b25037_moe[,c(1,10)],f.b25010_moe[,c(1,10)],f.b25064_moe[,c(1,8)],f.b25071_moe[,c(1,8)])

  f.AcsSt_moe <- f.AcsSt_moe[,c(1:18,20,22,24, 26)]

  f.AcsSt_moe[,2:22] <-as.numeric(as.character(f.AcsSt_moe[,2:22]))


  f.AcsSt_moe <- f.AcsSt_moe %>% mutate(
    People_TOT = b25033_moe008,
    People_1 = b25033_moe009,
    People_2_4 = b25033_moe010,
    People_5 = b25033_moe011,
    People_MH = b25033_moe012,
    People_OTH = b25033_moe013,
    Units_TOT = b25032_moe013,
    Units_1 =  sqrt(b25032_moe014^2 + b25032_moe015^2),
    Units_2_4 =  sqrt(b25032_moe016^2 + b25032_moe017^2),
    Units_5 = sqrt(b25032_moe018^2 + b25032_moe019 ^2 + b25032_moe020^2 + b25032_moe021^2),
    Units_MH = b25032_moe022,
    Units_OTH = b25032_moe023,
    Med_Yr = b25037_moe003,
    PPH = b25010_moe003,
    Med_Val = b25064_moe001,
    PCT_INC = b25071_moe001)

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

  # reassembling file
  f.AcsSt_Fin <- rbind(StPval,StUval,StRval)

  # Joining Fles
  f.RTHouse <- merge(f.AcsPl_Fin,f.AcsSt_Fin, by="var")

  #calculating statistical test
  f.RTHouse$ZScore <- ifelse(is.na(f.RTHouse$Pl_VAL_P), (abs(f.RTHouse$Pl_VAL - f.RTHouse$St_VAL)/sqrt((f.RTHouse$Pl_MOE^2) + (f.RTHouse$St_MOE^2))),
                             (abs(f.RTHouse$Pl_VAL_P - f.RTHouse$St_VAL_P)/sqrt((f.RTHouse$Pl_MOE_P^2) + (f.RTHouse$St_MOE_P^2))))
  f.RTHouse$SigDif <- ifelse(f.RTHouse$ZScore < 1, "No","Yes")

  # in table Order, formats and selecting output columns

  f.RTHouse <- f.RTHouse[c(16,11:15,9,4:8,1,2,10,3),]

  f.RTHouse$Pl_VAL_F <- ifelse(f.RTHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=0, big.mark=",")),
                               ifelse(f.RTHouse$var == "PPH", formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=2),
                                      ifelse(f.RTHouse$var == "Med_Yr", formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=0),
                                             ifelse(f.RTHouse$var == "PCT_INC",percent(f.RTHouse$Pl_VAL),formatC(as.numeric(f.RTHouse$Pl_VAL), format="f", digits=0, big.mark=",")
                                             ))))
  f.RTHouse$Pl_MOE_F <- ifelse(f.RTHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.RTHouse$Pl_MOE), format="f", digits=0, big.mark=",")),
                               ifelse(f.RTHouse$var == "PPH", formatC(as.numeric(f.RTHouse$Pl_MOE), format="f", digits=2),
                                      ifelse(f.RTHouse$var == "Med_Yr", formatC(as.numeric(f.RTHouse$Pl_MOE), format="f", digits=0),
                                             ifelse(f.RTHouse$var == "PCT_INC",percent(f.RTHouse$Pl_MOE),formatC(as.numeric(f.RTHouse$Pl_MOE), format="f", digits=0, big.mark=",")
                                             ))))
  f.RTHouse$St_VAL_F <- ifelse(f.RTHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.RTHouse$St_VAL), format="f", digits=0, big.mark=",")),
                               ifelse(f.RTHouse$var == "PPH", formatC(as.numeric(f.RTHouse$St_VAL), format="f", digits=2),
                                      ifelse(f.RTHouse$var == "Med_Yr", formatC(as.numeric(f.RTHouse$St_VAL), format="f", digits=0),
                                             ifelse(f.RTHouse$var == "PCT_INC",percent(f.RTHouse$St_VAL),formatC(as.numeric(f.RTHouse$St_VAL), format="f", digits=0, big.mark=",")
                                             ))))
  f.RTHouse$St_MOE_F <- ifelse(f.RTHouse$var == "Med_Val", paste0("$", formatC(as.numeric(f.RTHouse$St_MOE), format="f", digits=0, big.mark=",")),
                               ifelse(f.RTHouse$var == "PPH", formatC(as.numeric(f.RTHouse$St_MOE), format="f", digits=2),
                                      ifelse(f.RTHouse$var == "Med_Yr", formatC(as.numeric(f.RTHouse$St_MOE), format="f", digits=0),
                                             ifelse(f.RTHouse$var == "PCT_INC",percent(f.RTHouse$St_MOE),formatC(as.numeric(f.RTHouse$St_MOE), format="f", digits=0, big.mark=",")
                                             ))))

  f.RTHouse$Pl_VAL_PF <- percent(f.RTHouse$Pl_VAL_P*100)
  f.RTHouse$Pl_MOE_PF <- percent(f.RTHouse$Pl_MOE_P*100)
  f.RTHouse$St_VAL_PF <- percent(f.RTHouse$St_VAL_P*100)
  f.RTHouse$St_MOE_PF <- percent(f.RTHouse$St_MOE_P*100)

  f.RTHouse$Pl_VAL_PF <- ifelse(is.na(f.RTHouse$Pl_VAL_P),f.RTHouse$Pl_VAL_F,f.RTHouse$Pl_VAL_PF)
  f.RTHouse$Pl_MOE_PF <- ifelse(is.na(f.RTHouse$Pl_MOE_P),f.RTHouse$Pl_MOE_F,f.RTHouse$Pl_MOE_PF)
  f.RTHouse$St_VAL_PF <- ifelse(is.na(f.RTHouse$St_VAL_P),f.RTHouse$St_VAL_F,f.RTHouse$St_VAL_PF)
  f.RTHouse$St_MOE_PF <- ifelse(is.na(f.RTHouse$St_MOE_P),f.RTHouse$St_MOE_F,f.RTHouse$St_MOE_PF)

  f.RTHouse_Fin <- f.RTHouse[,c(1,14,15,18,19,16,17,20,21,13)]
  f.RTHouse_Fin[c(1,7),c(4,5,8,9,10)] <- ""
  f.RTHouse_Fin[c(13:16),c(4,5,8,9)] <- ""

  #Renaming rows and Columns
  f.RTHouse_Fin$var <- ifelse(f.RTHouse$var =="People_TOT", "Total Number of People in Rental Housing",
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
                                                                                                           ifelse(f.RTHouse$var =="Med_Yr","Median Year of Construction",
                                                                                                                  ifelse(f.RTHouse$var =="PPH","Average Number of Persons Per Household",
                                                                                                                         ifelse(f.RTHouse$var =="Med_Val","Median Gross Rent","Median Costs as a Percentage of Income"
                                                                                                                         )))))))))))))))

  names(f.RTHouse_Fin)  <-c("Variable",paste0("Value: ",ctyname),paste0("Margin of Error: ", ctyname),
                            paste0("Percentage Value: ",ctyname), paste0("Percentage Margin of Error: ", ctyname),
                            "Value: Colorado","Margin of Error: Colorado",
                            "Percentage Value: Colorado", "Percentage Margin of Error: Colorado", "Signifcant Difference?")


  m.RTHouse <- as.matrix(f.RTHouse_Fin)

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Count","Margin of Error","Percent","Margin of Error","Count","Margin of Error","Percent","Margin of Error","Significant Difference?")
  #Span Header

  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 4, "Colorado" = 4, " " = 1)

  # set vector names
  names(tblHead1) <- c(" ", ctyname, "Colorado", " ")

  if(oType == "html") {
  Housing_tab <- m.RTHouse %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrrrrrrrrr',
          caption="Characteristics of Rental Housing",
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

  outList <- list("table" = Housing_tab, "data" = f.RTHouse_Fin)
  return(outList)
}

 if(oType == "latex") {
   tabOut <-  kable(m.RTHouse,
                    col.names = names_spaced,
                    align=c("l",rep("r",9)),
                    caption="Characteristics of Rental Housing", row.names=FALSE,
                    format="latex", booktabs=FALSE)  %>%
     kable_styling(latex_options = "scale_down") %>%
     row_spec(0, align = "c") %>%
     add_header_above(header=tblHead1) %>%
     add_footnote(captionSrc("ACS",ACS))

  return(tabOut)
}
}
