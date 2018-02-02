#' housePRO  Produces the housing table
#'  CO Housing Unit Table
#'
#'  This function compares housing occupancy and vacancy rates for a place to the state
#'
#' @param fips The FIPS of the Place or County to use for the graph
#' @param ctyname The place Name
#' @param ACS  The American Community Survey Vintage
#' @param oType  Output type, html or latex
#' @param state Defaults to Colorado
#' @return kable formatted table and data file
#' @export
#'

housePRO=function(fips, ctyname, ACS, oType, state="08"){

  # Building ACS Place data table
  f.b25001 <- codemog_api(data="b25001", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")
  f.b25003 <- codemog_api(data="b25003", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")
  f.b25004 <- codemog_api(data="b25004", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")

  f.AcsPl <- cbind(f.b25001[,c(1,8)], f.b25003[,8:10],f.b25004[,8:15])

  f.AcsPl[,2:13]=as.numeric(as.character(f.AcsPl[,2:13]))

  f.AcsPl <- f.AcsPl %>% rename(Total=b25001001, Occupied=b25003001, Vacant=b25004001,
                                Owner = b25003002, Renter = b25003003, Seasonal = b25004006)%>%
    mutate(Other = sum(b25004002, b25004003, b25004004, b25004005, b25004007,b25004008))

  f.AcsPlace <- f.AcsPl[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)


  # Building ACS Place MOE table
  f.b25001_moe <- codemog_api(data="b25001_moe", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")
  f.b25003_moe <- codemog_api(data="b25003_moe", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")
  f.b25004_moe <- codemog_api(data="b25004_moe", db=ACS, geonum=paste("1", state, fips, sep=""),meta="no")

  f.AcsPl_moe <- cbind(f.b25001_moe[,c(1,8)], f.b25003_moe[,8:10],f.b25004_moe[,8:15])

  f.AcsPl_moe[,2:13]=as.numeric(as.character(f.AcsPl_moe[,2:13]))

  f.AcsPl_moe <- f.AcsPl_moe %>% rename(Total=b25001_moe001, Occupied=b25003_moe001, Vacant=b25004_moe001,
                                        Owner = b25003_moe002, Renter = b25003_moe003, Seasonal = b25004_moe006)%>%
    mutate(Other = sqrt(sum(b25004_moe002^2, b25004_moe003^2, b25004_moe004^2, b25004_moe005^2, b25004_moe007^2,b25004_moe008^2)))
  f.AcsPlace_moe <- f.AcsPl_moe[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)

  #Finalizing place table
  f.AcsPLFin <- merge(f.AcsPlace,f.AcsPlace_moe, by="var")
  f.AcsPLFin <- f.AcsPLFin[,c(1:3,5)]
  names(f.AcsPLFin) <- c("HCat", "geoname","PL_Value", "PL_MOE")
  #Calculating  proportions
  PL_Tot <- as.numeric(f.AcsPLFin[6,3])
  f.AcsPLFin$PL_VAL_Prop <- f.AcsPLFin$PL_Value/PL_Tot
  f.AcsPLFin$PL_MOE_Prop <- f.AcsPLFin$PL_MOE/PL_Tot
  f.AcsPLFin$PL_VAL_PCT <- percent(f.AcsPLFin$PL_VAL_Prop *100)
  f.AcsPLFin$PL_MOE_PCT <- percent(f.AcsPLFin$PL_MOE_Prop *100)

  f.AcsPLFin <- f.AcsPLFin[,c(1:3,5,7,4,6,8)]

  # Building ACS state data table
  f.b25001St <- codemog_api(data="b25001", db=ACS, geonum=paste("1", state,  sep=""),meta="no")
  f.b25003St <- codemog_api(data="b25003", db=ACS, geonum=paste("1", state,  sep=""),meta="no")
  f.b25004St <- codemog_api(data="b25004", db=ACS, geonum=paste("1", state,  sep=""),meta="no")

  f.AcsSt <- cbind(f.b25001St[,c(1,8)], f.b25003St[,8:10],f.b25004St[,8:15])

  f.AcsSt[,2:13]=as.numeric(as.character(f.AcsSt[,2:13]))

  f.AcsSt <- f.AcsSt %>% rename(Total=b25001001, Occupied=b25003001, Vacant=b25004001,
                                Owner = b25003002, Renter = b25003003, Seasonal = b25004006)%>%
    mutate(Other = sum(b25004002, b25004003, b25004004, b25004005, b25004007,b25004008))

  f.AcsState <- f.AcsSt[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)


  # Building ACS State MOE table
  f.b25001St_moe <- codemog_api(data="b25001_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")
  f.b25003St_moe <- codemog_api(data="b25003_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")
  f.b25004St_moe <- codemog_api(data="b25004_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")

  f.AcsSt_moe <- cbind(f.b25001St_moe[,c(1,8)], f.b25003St_moe[,8:10],f.b25004St_moe[,8:15])

  f.AcsSt_moe[,2:13]=as.numeric(as.character(f.AcsSt_moe[,2:13]))

  f.AcsSt_moe <- f.AcsSt_moe %>% rename(Total=b25001_moe001, Occupied=b25003_moe001, Vacant=b25004_moe001,
                                        Owner = b25003_moe002, Renter = b25003_moe003, Seasonal = b25004_moe006)%>%
    mutate(Other = sqrt(sum(b25004_moe002^2, b25004_moe003^2, b25004_moe004^2, b25004_moe005^2, b25004_moe007^2,b25004_moe008^2)))
  f.AcsState_moe <- f.AcsSt_moe[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)


  #Finalizing state table
  f.AcsStFin <- merge(f.AcsState,f.AcsState_moe, by="var")
  f.AcsStFin <- f.AcsStFin[,c(1:3,5)]
  names(f.AcsStFin) <- c("HCat", "geoname","ST_Value", "ST_MOE")
  #Calculating  proportions
  ST_Tot <- as.numeric(f.AcsStFin[6,3])
  f.AcsStFin$ST_VAL_Prop <- f.AcsStFin$ST_Value/ST_Tot
  f.AcsStFin$ST_MOE_Prop <- f.AcsStFin$ST_MOE/ST_Tot
  f.AcsStFin$ST_VAL_PCT <- percent(f.AcsStFin$ST_VAL_Prop *100)
  f.AcsStFin$ST_MOE_PCT <- percent(f.AcsStFin$ST_MOE_Prop *100)

  f.AcsStFin <- f.AcsStFin[,c(1:3,5,7,4,6,8)]

  # Assembling Combined Tab: f.longtab

  f.longTab <- merge(f.AcsPLFin,f.AcsStFin, by="HCat")

  # Calculating Statistical Test
  f.longTab$ZScore <- (abs(f.longTab$PL_VAL_Prop - f.longTab$ST_VAL_Prop)/
                         sqrt((f.longTab$PL_MOE_Prop^2) + (f.longTab$ST_MOE_Prop^2)))
  f.longTab$Sig_Diff <- ifelse(f.longTab$ZScore < 1,"No","Yes")


  f.longTab$HCat <- ifelse(f.longTab$HCat == "Total","Total Housing Units",
                           ifelse(f.longTab$HCat == "Occupied","Occupied Housing Units",
                                  ifelse(f.longTab$HCat == "Owner", "Owner-Occupied Units",
                                         ifelse(f.longTab$HCat == "Renter", "Renter-Occupied Units",
                                                ifelse(f.longTab$HCat == "Vacant", "Vacant Housing Units",
                                                       ifelse(f.longTab$HCat == "Seasonal","Seasonal Units","All Other Rental Units"))))))

  #Reordering Table and prepating output

  f.HouseTab <- f.longTab[c(6,1,3,4,5,2,7),c(1,3,5,8,10,12,15,17)]
  f.HouseTab[2] <- comma(f.HouseTab[2])
  f.HouseTab[5] <- comma(f.HouseTab[5])
  f.HouseTab[1,3] <- ""
  f.HouseTab[1,4] <- ""
  f.HouseTab[1,6] <- ""
  f.HouseTab[1,7] <- ""
  f.HouseTab[1,8] <- ""

  m.House <- as.matrix(f.HouseTab)

  names(f.HouseTab) <- c("Housing Type", paste0("Housing Units: ",ctyname),
                         paste0("Percentage: ",ctyname),paste0("Margin of Error: ",ctyname),
                         "Housing Units: Colorado", "Percentage: Colorado", "Margin of Error: Colorado",
                         "Signficant Difference")

  # Setting up table

  #Column Names
  names_spaced <- c("Housing Type","Count","Percent","Margin of Error","Count","Percent","Margin of Error","Significant Difference?")
  #Span Header

  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 3, "Colorado" = 3, " " = 1)

  # set vector names
  names(tblHead1) <- c(" ", ctyname, "Colorado", " ")

  if(oType == "html") {
  Housing_tab <- m.House %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrrrrrrr',
          caption="Housing Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "45em",bold = T) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width ="5em") %>%
    column_spec(4, width ="5em") %>%
    column_spec(5, width ="5em") %>%
    column_spec(6, width ="5em") %>%
    column_spec(7, width ="5em") %>%
    column_spec(8, width ="5em") %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))

  outList <- list("table" = Housing_tab, "data" = f.HouseTab)
  return(outList)
 }

  if(oType == "latex") {
  tabOut <- m.House %>% kable(
        col.names = names_spaced,
        align=c("l",rep("r",7)),
        caption="Housing Comparison", row.names=FALSE,
        format="latex", booktabs=TRUE)  %>%
        kable_styling() %>%
        column_spec(1, width = "2in") %>%
        column_spec(2, width ="0.55in") %>%
        column_spec(3, width ="0.55in") %>%
        column_spec(4, width ="0.55in") %>%
        column_spec(5, width ="0.55in") %>%
        column_spec(6, width ="0.55in") %>%
        column_spec(7, width ="0.55in") %>%
        column_spec(8, width ="0.75in") %>%
        add_indent(c(3:7)) %>%
        add_header_above(header=tblHead1) %>%
        add_footnote(captionSrc("ACS",ACS))


  return(tabOut)
  }
}
