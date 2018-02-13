#' HouseVal Table Showing the Median House value for Owner-occupied housing,
#'     the Median Gross Rent, and Median Costs as a Percentage of Income for
#'     Owners and Renters for a place and the State of Colorado
#'
#' @param fips The FIPS of the Place or County to use for the graph
#' @param ctyname The place Name
#' @param ACS  The American Community Survey Vintage
#' @param oType Output Type, html or latex
#' @param state defaults to Colorado
#' @return kable formatted  table and data file
#' @export
#'

HouseVal <- function(fips, ctyname, ACS, oType, state="08"){

  # Raw Place data Owners
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25092 <- codemog_api(data="b25092", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  # Raw Place data Renters
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25071 <- codemog_api(data="b25071", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  f.AcsPl <- cbind(f.b25077[,c(1,8)],f.b25092[,c(1,8)],f.b25064[,c(1,8)],f.b25071[,c(1,8)])

  f.AcsPl <-   f.AcsPl[,c(1,2,4,6,8)]

  f.AcsPl[,2:5] <-as.numeric(as.character(f.AcsPl[,2:5]))


  f.AcsPl <- f.AcsPl %>% mutate(
    Med_Val = b25077001,
    PCT_INCOO = b25092001,
    Med_Rent = b25064001,
    PCT_INCRT = b25071001)


  f.AcsPlL <- f.AcsPl %>% gather(var, ACS, Med_Val:PCT_INCRT, -geoname)

  f.AcsPlL <- f.AcsPlL[,c(1,6,7)]

  # Raw Place Owners  MOE
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25092_moe <- codemog_api(data="b25092_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income

  #Raw place Renters MOE
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # Median Value
  f.b25071_moe <- codemog_api(data="b25071_moe", db=ACS, geonum=paste0("1", state, fips),meta="no") # costs as % of Income


  f.AcsPl_moe <- cbind(f.b25077_moe[,c(1,8)],f.b25092_moe[,c(1,8)],f.b25064_moe[,c(1,8)],f.b25071_moe[,c(1,8)])

  f.AcsPl_moe <- f.AcsPl_moe[,c(1,2,4,6,8)]

  f.AcsPl_moe[,2:5] <-as.numeric(as.character(f.AcsPl_moe[,2:5]))


  f.AcsPl_moe <- f.AcsPl_moe %>% mutate(
    Med_Val = b25077_moe001,
    PCT_INCOO = b25092_moe001,
    Med_Rent = b25064_moe001,
    PCT_INCRT = b25071_moe001)

  f.AcsPlL_moe <- f.AcsPl_moe %>% gather(var, MOE, Med_Val:PCT_INCRT, -geoname)

  f.AcsPlL_moe <- f.AcsPlL_moe[,c(1,6,7)]

  f.AcsPl_Fin <- merge(f.AcsPlL,f.AcsPlL_moe,by ="var")

  f.AcsPl_Fin <- f.AcsPl_Fin[c(2,3,1,4),c(1:3,5)]
  names(f.AcsPl_Fin) <- c("var","Place","Pl_VAL", "Pl_MOE")

  # Assembling State Data

  # Raw State data Owners
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25092 <- codemog_api(data="b25092", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  # Raw Place data Renters
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25071 <- codemog_api(data="b25071", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  f.AcsSt <- cbind(f.b25077[,c(1,8)],f.b25092[,c(1,8)],f.b25064[,c(1,8)],f.b25071[,c(1,8)])

  f.AcsSt <-   f.AcsSt[,c(1,2,4,6,8)]

  f.AcsSt[,2:5] <-as.numeric(as.character(f.AcsSt[,2:5]))


  f.AcsSt <- f.AcsSt %>% mutate(
    Med_Val = b25077001,
    PCT_INCOO = b25092001,
    Med_Rent = b25064001,
    PCT_INCRT = b25071001)


  f.AcsStL <- f.AcsSt %>% gather(var, ACS, Med_Val:PCT_INCRT, -geoname)

  f.AcsStL <- f.AcsStL[,c(1,6,7)]

  # Raw State Owners  MOE
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25092_moe <- codemog_api(data="b25092_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income

  #Raw Stace Renters MOE
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25071_moe <- codemog_api(data="b25071_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income


  f.AcsSt_moe <- cbind(f.b25077_moe[,c(1,8)],f.b25092_moe[,c(1,8)],f.b25064_moe[,c(1,8)],f.b25071_moe[,c(1,8)])

  f.AcsSt_moe <- f.AcsSt_moe[,c(1,2,4,6,8)]

  f.AcsSt_moe[,2:5] <-as.numeric(as.character(f.AcsSt_moe[,2:5]))


  f.AcsSt_moe <- f.AcsSt_moe %>% mutate(
    Med_Val = b25077_moe001,
    PCT_INCOO = b25092_moe001,
    Med_Rent = b25064_moe001,
    PCT_INCRT = b25071_moe001)

  f.AcsStL_moe <- f.AcsSt_moe %>% gather(var, MOE, Med_Val:PCT_INCRT, -geoname)

  f.AcsStL_moe <- f.AcsStL_moe[,c(1,6,7)]

  f.AcsSt_Fin <- merge(f.AcsStL,f.AcsStL_moe,by ="var")

  f.AcsSt_Fin <- f.AcsSt_Fin[c(2,3,1,4),c(1:3,5)]
  names(f.AcsSt_Fin) <- c("var","Place","St_VAL", "St_MOE")



  # Joining Fles
  f.HouseVal <- merge(f.AcsPl_Fin,f.AcsSt_Fin, by="var")
  f.HouseVal <- f.HouseVal[,c(1:4,6,7)]

  #calculating statistical test
  f.HouseVal$ZScore <- abs(f.HouseVal$Pl_VAL - f.HouseVal$St_VAL)/sqrt((f.HouseVal$Pl_MOE^2) + (f.HouseVal$St_MOE^2))
  f.HouseVal$SigDif <- ifelse(f.HouseVal$ZScore < 1, "No","Yes")

  # in table Order, formats and selecting output columns
  f.HouseVal <- f.HouseVal[c(2,3,1,4),c(1:6,8)]

  f.HouseVal$Pl_VAL_F <- ifelse(f.HouseVal$var == "Med_Val", paste0("$", formatC(as.numeric(f.HouseVal$Pl_VAL), format="f", digits=0, big.mark=",")),
                                ifelse(f.HouseVal$var == "Med_Rent",paste0("$", formatC(as.numeric(f.HouseVal$Pl_VAL), format="f", digits=0, big.mark=",")),
                                       percent(f.HouseVal$Pl_VAL)))


  f.HouseVal$Pl_MOE_F <- ifelse(f.HouseVal$var == "Med_Val", paste0("$", formatC(as.numeric(f.HouseVal$Pl_MOE), format="f", digits=0, big.mark=",")),
                                ifelse(f.HouseVal$var == "Med_Rent", paste0("$", formatC(as.numeric(f.HouseVal$Pl_MOE), format="f", digits=0, big.mark=",")),
                                       percent(f.HouseVal$Pl_MOE)))

  f.HouseVal$St_VAL_F <- ifelse(f.HouseVal$var == "Med_Val", paste0("$", formatC(as.numeric(f.HouseVal$St_VAL), format="f", digits=0, big.mark=",")),
                                ifelse(f.HouseVal$var == "Med_Rent",paste0("$", formatC(as.numeric(f.HouseVal$St_VAL), format="f", digits=0, big.mark=",")),
                                       percent(f.HouseVal$St_VAL)))


  f.HouseVal$St_MOE_F <- ifelse(f.HouseVal$var == "Med_Val", paste0("$", formatC(as.numeric(f.HouseVal$St_MOE), format="f", digits=0, big.mark=",")),
                                ifelse(f.HouseVal$var == "Med_Rent", paste0("$", formatC(as.numeric(f.HouseVal$St_MOE), format="f", digits=0, big.mark=",")),
                                       percent(f.HouseVal$St_MOE)))


  f.HouseVal <- f.HouseVal[,c(1,8:11,7)]

  #Renaming rows and Columns

  f.HouseVal$var <-  ifelse(f.HouseVal$var == "Med_Val","Median Home Value (Current $)",
                            ifelse(f.HouseVal$var == "Med_Rent","Median Gross Rent (Current $)",
                                   "Housing Costs as a Percentage of Income"))




  OOLine <- c("Owner Occupied Housing",rep(" ",5))
  RTLine <- c("Rental Housing",rep(" ",5))
  m.HouseValO <- as.matrix(f.HouseVal[c(1,2),])

  m.HouseValR <- as.matrix(f.HouseVal[c(3,4),])


  m.HOwn <- rbind(OOLine,m.HouseValO)
  m.HRent <- rbind(RTLine,m.HouseValR)
  m.HouseVal <- rbind(OOLine,m.HouseValO,RTLine,m.HouseValR)


  f.HouseVal <- as.data.frame(m.HouseVal)
  names(f.HouseVal)  <-c("Variable",paste0("Value: ",ctyname),paste0("Margin of Error: ", ctyname),
                         "Value: Colorado","Margin of Error: Colorado", "Signifcant Difference?")

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Value","Margin of Error","Value","Margin of Error","Sig. Diff.?")
  #Span Header

  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 2, "Colorado" = 2, " " = 1)

  # set vector names
  names(tblHead1) <- c(" ", ctyname, "Colorado", " ")

  if(oType == "html") {
    Housing_tab1 <- m.HOwn %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrrr',
            caption="Comparative Owner-Occupied Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      column_spec(6, width ="0.5in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    Housing_tab2 <- m.HRent%>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrrr',
            caption="Comparative Rental Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      column_spec(6, width ="0.5in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    outList <- list("table0" = Housing_tab1, "tableR" = Housing_tab2, "data" = f.HouseVal)
    return(outList)
  }

  if(oType == "latex") {

    tabOut <-  kable(m.HouseVal,
                     col.names = names_spaced,
                     align=c("lrrrrr"),
                     caption="Comparison of Housing Values", row.names=FALSE,
                     format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      row_spec(0, align = "c") %>%
      row_spec(1, bold = TRUE, italic = TRUE) %>%
      row_spec(4, bold = TRUE, italic = TRUE) %>%
      column_spec(1, width = "2.5in") %>%
      column_spec(2, width = "0.5in") %>%
      column_spec(3, width ="0.5in") %>%
      column_spec(4, width ="0.5in") %>%
      column_spec(5, width ="0.5in") %>%
      column_spec(6, width ="0.5in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    return(tabOut)
  }
}
