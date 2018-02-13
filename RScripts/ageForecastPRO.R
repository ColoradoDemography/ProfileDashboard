#' ageForecastPRO Produces a Age Forecast data set and chart
#'
#' Copied from "ms_popage" in codemgprofile, Modified by AB 12/2017
#' Creates a Chart comparing Forecast Population Growth by Age in Colorado.
#'
#' Uses the data from the State Demography Office package codemog to
#' create a graph showing projected population  changes by Age for each Colorado county from
#' 2010 to 2025.
#' The chart is modified from the original.  Now, we show three bars, one for each series.
#'
#' @param fips is the fips code for the county being examined
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

ageForecastPRO=function(fips, stYr, mYr, eYr, base=12, agegroup="ten"){

  fips=as.numeric(fips)

  yrs=c(stYr, mYr, eYr)

  d=county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    group_by(agecat)%>%
    arrange(countyfips, year)


  barCol <- c("#82BC00", "#009ADD", "#5C666F")
  pltTitle <- paste0("Age Forecast")
  subTitle <- paste0(as.character(d[1,2]), " County: Change in Population by Age: ",stYr," to ",eYr )
  names(d)[3] <- "Year"
  d$Year <- as.factor(d$Year)
  d$Year <- factor(d$Year, levels=yrs)

 #Setting MaxValue
  maxVal <- ifelse(max(d$totalpopulation) < 1000,1000,
            ifelse(max(d$totalpopulation) < 2000,2000,
            ifelse(max(d$totalpopulation) < 3000,3000,
            ifelse(max(d$totalpopulation) < 4000,4000,
            ifelse(max(d$totalpopulation) < 5000,5000,
            ifelse(max(d$totalpopulation) < 10000,10000,
            ifelse(max(d$totalpopulation) < 15000,15000,
            ifelse(max(d$totalpopulation) < 25000,25000,
            ifelse(max(d$totalpopulation) < 50000,50000, max(d$totalpopulation) + 25000)))))))))

  p <- d %>%
    ggplot(aes(x=agecat, y=totalpopulation, fill=Year))+
    geom_bar(stat="identity",color="black", position = position_dodge()) +
    scale_y_continuous(label=comma, expand = c(0, 0))+
    scale_fill_manual(values=barCol) +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Age Group",
         y= "Total Population") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")

  #Regrouping Data

  dWide <- spread(d, Year, totalpopulation)
  names(dWide)[4] <- paste0("totalPop_",stYr)
  names(dWide)[5] <- paste0("totalPop_",mYr)
  names(dWide)[6] <- paste0("totalPop_",eYr)

  dWide[4] <- round(dWide[4], digits = 0)
  dWide[5] <- round(dWide[5], digits = 0)
  dWide[6] <- round(dWide[6], digits = 0)

  dWide$county <- paste0(dWide$county," County")

  #binding List
  outList <- list("plot"= p, "data" = dWide)

  return(outList)
}
