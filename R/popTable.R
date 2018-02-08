#' popTable The population table showing the annual growth rate in the Population Section
#'
#' @param cty short FIPS code, without the state code
#' @param ctyname the place name
#' @param sYr Start Year
#' @param eYr End year
#' @param oType Output Type, html or latex
#' @return kable formatted  table and data file
#' @export
#'
popTable <- function(cty,ctyname,sYr,eYr,oType) {
  #outputs the population trend table in the population section..

  state <- "Colorado"
  cntynum <- as.numeric(cty)
  yrs <- as.character(setYrRange(sYr,eYr))
  #State Population and Growth Rate
  popCO=county_profile(0, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=percent(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)))),
           Population=totalpopulation)
  #County Population and Growth Rate
  popCounty=county_profile(cntynum, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    arrange(year)%>%
    mutate(name=county,
           year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=percent(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)))),
           Population=totalpopulation)



  # Creating Output Table
  f.County <- popCounty[,c(3,5:7)]
  f.CO <- popCO[,c(1,5:7)]
  f.Out <- merge(f.County,f.CO,by="year")
  f.Out2 <- f.Out
  f.Out2[,4] <- format(f.Out2[,4],big.mark=",")
  f.Out2[,7] <- format(f.Out2[,7],big.mark=",")

  m.OutTab <- as.matrix(f.Out2[,c(1,4,3,7,6)])
  m.OutTab <- gsub("NA%","",m.OutTab)


  names_spaced <- c("Year","Population","Annual Growth Rate","Population","Annual Growth Rate")


  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, state = 2)

  # set vector names
  names(tblHead) <- c(" ", ctyname,state)

  if(oType == "html") {
    # Creating Final Table (kable)
    OutTab  <- m.OutTab %>%
      kable(format='html', table.attr='class="myTable"',
            caption = "Population Trend",
            digits=1,
            row.names=FALSE,
            align='lccccc',
            col.names = names_spaced,
            escape = FALSE) %>%
      kable_styling(bootstrap_options = "condensed") %>%
      column_spec(1, bold = T) %>%
      column_spec(2, width = "13em") %>%
      column_spec(3, width ="18em") %>%
      column_spec(4, width = "13em") %>%
      column_spec(5, width = "18em") %>%
      add_header_above(header=tblHead)  %>%
      add_footnote(captionSrc("SDO",""))

    # Creating Final Data Set
    f.Out2 <- f.Out2[,c(1,4,3,7,6)]
    names(f.Out2) <- c("Year",paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                      "Population: Colorado","Growth Rate: Colorado")


    # bind list
    outList <- list("table" = OutTab,"data" = f.Out2)

    return(outList)
  }


  if(oType == "latex") {
    # building Text
    # Max Pop
    rows <- nrow(f.Out)
    maxPop <- max(f.Out[,4])
    maxYear <- as.numeric(f.Out[which(f.Out[,4] == maxPop),1])
    sVal <- f.Out[1,4]
    eVal <- f.Out[rows,4]
    popDiff <- as.numeric(eVal) - as.numeric(sVal)
    pctDiff <- percent((popDiff/as.numeric(sVal))*100)
    chgType <- ifelse(popDiff < 0,"decreased",
               ifelse(popDiff > 0, "increased", "stayed the same"))

    if(popDiff == 0) {
      outTxt <- paste0(ctyname," population ",chgType," between ", sYr, " and ",eYr,".")

    } else {
    outTxt <- paste0("The population of ",ctyname," ",chgType," by ", format(popDiff,big.mark=",")," (", pctDiff,") between ", sYr, " and ",eYr,".  Since 1990, the peak of ",ctyname,"'s population was ",
                     format(maxPop,big.mark=",")," in ", maxYear,".")
    }


  outKable <- kable(m.OutTab, col.names = names_spaced ,
               caption="Population Trend",row.names = FALSE, align="c",
               format ="latex", booktabs=TRUE)  %>%
               kable_styling(latex_options="HOLD_position") %>%
                column_spec(1, width = "0.5in") %>%
                column_spec(2, width = "0.75in") %>%
                column_spec(3, width = "0.75in") %>%
                column_spec(4, width = "0.75in") %>%
                column_spec(5, width = "0.75in") %>%
                add_header_above(header=tblHead) %>%
               add_footnote(captionSrc("SDO",""))
  outList <- list("table" = outKable,"text" = outTxt)
  return(outList)
  }

}
