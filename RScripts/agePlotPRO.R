#' agePlotPRO Creates a Chart comparing The age distribution of a selected place to the state for a simgle year
#'
#' @param ctyfips is the numeric ctyfips code for the main area to be compared
#' @param ctyname is the county place name 
#' @param placefips is the numeric ctyfips code for the main area to be compared
#' @param placename is the municipality name from input$unit
#' @param ACS is the code for the current Americn Ciommunity Survey data set (for municipalities)
#' @param state is the numeric state code , it defaults to 0 in the county_sya call
#' @param yrs is the single year value to be extracted by county_sya
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

agePlotPRO  <- function(ctyfips, ctyname, placefips, placename, ACS, state=0, yrs, base=10, agegroup="ten") {

  if(nchar(placefips) == 0) { # County data call
    ctyfips <- as.numeric(ctyfips)
    #Creating Place data File
    f.place =county_sya(ctyfips, yrs)%>%
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
    f.AgePlot$geoname <- f.AgePlot$county
    citsrc1 <- "SDO"
    citsrc2 <- ""
    
    subTitle <- ctyname
    f.AgePlot$geoname <- factor(f.AgePlot$geoname, levels=c(ctyname, "Colorado"))
    
    x <- merge(f.place, f.state, by="agecat")
    f.AgePlot2 <- x[,c(1,5,7,12,14)]
    f.AgePlot2$totalpopulation.x <- format(round(f.AgePlot2$totalpopulation.x,digits=0),big.mark=",")
    f.AgePlot2$totalpopulation.y <- format(round(f.AgePlot2$totalpopulation.y,digits=0),big.mark=",")
    names(f.AgePlot2) <- c("Age Category",  paste0("Population: ",ctyname), paste0("Population Percentage: ",ctyname),
                           "Population: Colorado", "Population percentage: Colorado")
  }

  if(nchar(placefips) != 0) { # this is municpal Call from the ACS
    state <- "08"
    f.place <- codemog_api(data="b01001",db=ACS,geonum=paste("1",state , placefips,sep=""),meta="no")
    f.place[,8:56]=as.numeric(as.character(f.place[,8:56]))
    f.place2 <- f.place %>%
      mutate(
        a0009 = b01001003 + b01001004 + b01001027 + b01001028,
        a1019 = b01001005 + b01001006 + b01001007 + b01001029 + b01001030 + b01001031,
        a2029 = b01001008 + b01001009 + b01001010 + b01001011 + b01001032 + b01001033 + b01001034 + b01001035,
        a3039 = b01001012 + b01001013 + b01001036 + b01001037,
        a4049 = b01001014 + b01001015 + b01001038 + b01001039,
        a5059 = b01001016 + b01001017 + b01001040 + b01001041,
        a6069 = b01001018 + b01001019 + b01001020 + b01001021 + b01001042 + b01001043 + b01001044 + b01001045,
        a7079 = b01001022 + b01001023 + b01001046 + b01001047,
        a8084 = b01001024 + b01001048,
        a85 = b01001025 + b01001049) %>%
        select(geoname:geonum,a0009:a85)%>%
      gather(ageLevel, value, a0009:a85, factor_key=TRUE)%>%  #Needed to change this part of the call
      mutate(agecat=ordered(as.factor(ageLevel), 
             levels=c("a0009", "a1019", "a2029", "a3039", "a4049",
                      "a5059", "a6069", "a7079", "a8084", "a85"),
            labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                     "50 to 59", "60 to 69", "70 to 79", "80 to 84", "85 and over")),
            age_Value = value,
            age_Prop = (value/sum(value))*100)
    f.place2 <- f.place2[,c(1,10:12)] 
    f.place2$geoname <- placename
    
    f.county <- codemog_api(data="b01001",db=ACS,geonum=paste("1",state , ctyfips,sep=""),meta="no")
    f.county[,8:56]=as.numeric(as.character(f.county[,8:56]))
    f.county2 <- f.county %>%
      mutate(
        a0009 = b01001003 + b01001004 + b01001027 + b01001028,
        a1019 = b01001005 + b01001006 + b01001007 + b01001029 + b01001030 + b01001031,
        a2029 = b01001008 + b01001009 + b01001010 + b01001011 + b01001032 + b01001033 + b01001034 + b01001035,
        a3039 = b01001012 + b01001013 + b01001036 + b01001037,
        a4049 = b01001014 + b01001015 + b01001038 + b01001039,
        a5059 = b01001016 + b01001017 + b01001040 + b01001041,
        a6069 = b01001018 + b01001019 + b01001020 + b01001021 + b01001042 + b01001043 + b01001044 + b01001045,
        a7079 = b01001022 + b01001023 + b01001046 + b01001047,
        a8084 = b01001024 + b01001048,
        a85 = b01001025 + b01001049) %>%
      select(geoname:geonum,a0009:a85)%>%
      gather(ageLevel, value, a0009:a85, factor_key=TRUE)%>%  #Needed to change this part of the call
      mutate(agecat=ordered(as.factor(ageLevel), 
                            levels=c("a0009", "a1019", "a2029", "a3039", "a4049",
                                     "a5059", "a6069", "a7079", "a8084", "a85"),
                            labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                                     "50 to 59", "60 to 69", "70 to 79", "80 to 84", "85 and over")),
             age_Value = value,
             age_Prop = (value/sum(value))*100)
    f.county2 <- f.county2[,c(1,10:12)]
    f.county2$geoname <- ctyname
    
    # Creating Plot data file
    f.AgePlot <- rbind(f.place2, f.county2)
    citsrc1 <- "ACS"
    citsrc2 <- ACS
    
    subTitle <- placename
    f.AgePlot$geoname <- factor(f.AgePlot$geoname, levels=c(placename, ctyname))
    
    f.AgePlot2 <- merge(f.place2, f.county2, by="agecat")
    f.AgePlot2 <- f.AgePlot2[,c(1,3,4,6,7)]
    f.AgePlot2$age_Value.x <- format(round(f.AgePlot2$age_Value.x,digits=0),big.mark=",")
    f.AgePlot2$age_Prop.x <- percent(f.AgePlot2$age_Prop.x)
    f.AgePlot2$age_Value.y <- format(round(f.AgePlot2$age_Value.y,digits=0),big.mark=",")
    f.AgePlot2$age_Prop.y <- percent(f.AgePlot2$age_Prop.y)
    
    
    names(f.AgePlot2) <- c("Age Category",  paste0("Population: ",placename), paste0("Population Percentage: ",placename),
                           paste0("Population: ",ctyname), paste0("Population percentage: ",ctyname))
    
    
  }

  #Preparing Plot

  barCol <- c("#6EC4E8","#00953A")
  pltTitle <- paste0("Population Distribution by Age for ",yrs)


  axs <- setAxis(f.AgePlot$age_Prop)

  AgePlot <- f.AgePlot %>%
    ggplot(aes(x=agecat, y=age_Prop, fill=geoname))+
    geom_bar(stat="identity",color="black", position = position_dodge()) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks=axs$yBrk, label=percent, expand = c(0, 0))+
    scale_fill_manual(values=barCol, name="Geography") +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc(citsrc1,citsrc2),
         x = "Age Group",
         y= "Percentage of Total Population") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")


  outList <- list("plot" = AgePlot, "data" = f.AgePlot2)
  return(outList)
}
