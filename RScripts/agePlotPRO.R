#' agePlotPRO Creates a Chart comparing The age distribution of a selected place to the state for a simgle year
#'
#' @param fips is the numeric fips code for the main area to be compared
#' @param ctyname is the cplace name from input$unit
#' @param state is the numeric state code , it defaults to 0 in the county_sya call
#' @param yrs is the single year value to be extracted by county_sya
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

agePlotPRO  <- function(fips, ctyname, state=0, yrs, base=10, agegroup="ten") {

  #Creating Place data File
  f.place =county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    arrange(countyfips, year) %>%
    mutate(popTot = sum(totalpopulation)) %>%
    group_by(agecat, add=TRUE) %>%
    mutate(age_Pct = percent((totalpopulation/sum(popTot))*100)) %>%
    mutate(age_Prop = (totalpopulation/sum(popTot))*100)

  f.place$county <- ctyname

  #Creating State Data file
  f.state =county_sya(state, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    arrange(countyfips, year) %>%
    mutate(popTot = sum(totalpopulation)) %>%
    group_by(agecat, add=TRUE) %>%
    mutate(age_Pct = percent((totalpopulation/sum(popTot))*100)) %>%
    mutate(age_Prop = (totalpopulation/sum(popTot))*100)

  # Creating Plot data file
  f.AgePlot <- rbind(f.place, f.state)

  #Preparing Plot

  barCol <- c("#6EC4E8","#00953A")
  pltTitle <- paste0("Population Distribution by Age for ",yrs)
  subTitle <- ctyname
  f.AgePlot$county <- factor(f.AgePlot$county, levels=c(ctyname, "Colorado"))
  maxProp <- max(f.AgePlot$age_Prop) + 10

  AgePlot <- f.AgePlot %>%
    ggplot(aes(x=agecat, y=age_Prop, fill=county))+
    geom_bar(stat="identity",color="black", position = position_dodge()) +
    scale_y_continuous(label=percent, expand = c(0, 0))+
    scale_fill_manual(values=barCol, name="Geography") +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Age Group",
         y= "Percentage of Total Population") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")



  x <- merge(f.place, f.state, by="agecat")
  f.AgePlot2 <- x[,c(1,4,5,7,12,14)]
  f.AgePlot2[3] <- round(f.AgePlot2[3],digits=0)
  f.AgePlot2[5] <- round(f.AgePlot2[5],digits=0)
  names(f.AgePlot2) <- c("Age Category", "Year", paste0("Population: ",ctyname), paste0("Population Percentage: ",ctyname),
                         "Population: Colorado", "Population percentage: Colorado")

  outList <- list("plot" = AgePlot, "data" = f.AgePlot2)
  return(outList)
}
