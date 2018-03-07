#' educPRO Creates a Chart comparing educational attainment of two areas
#'
#' Modified from ms_ed in codemogProfile AB 12/2017
#' Uses the codemog_api function to access ACS data (defaults to 13-5yr) to create a ggplot2 chart for
#' use in profiles.
#'
#' @param ctyfips is the fips code for the selected county
#' @param ctyname is the name of the selected county
#' @param placefips is the fips code for the selected municipality
#' @param placename is the name of the selected municipality
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic aand data file
#' @export

educPRO <- function(listID, ACS){
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  
  state="08"
  base=10
  
  
  #county Education Value
  d13cty <- codemog_api(data="b15003",db=ACS,geonum=paste("1",state , ctyfips,sep=""),meta="no")
  d13cty[,7:32]=as.numeric(as.character(d13cty[,7:32]))
  d13ctyVAL <- d13cty%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025) %>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than\nHigh School",
                                    "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or\nProfessional Degree")))
  
  
  # Place Education MOE
  d13ctym <- codemog_api(data="b15003_moe",db=ACS,geonum=paste("1",state , ctyfips,sep=""),meta="no")
  d13ctym[,7:32]=as.numeric(as.character(d13ctym[,7:32]))
  
  #Calculating the summary MOE
  d13ctyMOE <- d13ctym %>%
    mutate(ed1=sqrt(b15003_moe002^2+b15003_moe003^2+b15003_moe004^2+b15003_moe005^2+b15003_moe006^2+b15003_moe007^2+
                      b15003_moe008^2+b15003_moe009^2+b15003_moe010^2+b15003_moe011^2+b15003_moe012^2+b15003_moe013^2+
                      b15003_moe014^2+b15003_moe015^2+b15003_moe016^2),
           ed2=sqrt(b15003_moe017^2+b15003_moe018^2),
           ed3=sqrt(b15003_moe019^2+b15003_moe020^2+b15003_moe021^2),
           ed4=b15003_moe022,
           ed5=sqrt(b15003_moe023^2+b15003_moe024^2+b15003_moe025^2)) %>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than\nHigh School",
                                    "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or\nProfessional Degree")))
  
  
  #Preparing data
  names(d13ctyMOE)[9] <- "MOE"
  d13ctyVAL2 <- d13ctyVAL[,c(1,8,10,9)]
  d13ctyMOE2 <- d13ctyMOE[,c(8,9)]
  
  d13ctyF <- merge(d13ctyVAL2,d13ctyMOE2,by="EdLevel")
  f.d13ctyFin <- d13ctyF %>%
    mutate(c_propVAL = value/sum(value),
           c_propMOE = MOE/sum(value))
  
  f.d13ctyFin$c_ciLOW  <- f.d13ctyFin$c_propVAL - f.d13ctyFin$c_propMOE
  f.d13ctyFin$c_ciHIGH <- f.d13ctyFin$c_propVAL + f.d13ctyFin$c_propMOE
  f.d13ctyFin$c_pctVAL <- percent(f.d13ctyFin$c_propVAL *100)
  f.d13ctyFin$c_pctMOE <- percent(f.d13ctyFin$c_propMOE *100)
  f.d13ctyFin$c_pctLOW <- percent(f.d13ctyFin$c_ciLOW *100)
  f.d13ctyFin$c_pctHIGH <- percent(f.d13ctyFin$c_ciHIGH *100)
  
  f.d13ctyFinM <- f.d13ctyFin[, c(3,2,6,8,9)]
  names(f.d13ctyFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")
  f.d13ctyFinM$geoname <- ctyname
  
  #State Education Values
  d13ST <- codemog_api(data="b15003",db=ACS,geonum=paste("1",state , sep=""),meta="no")
  d13ST[,7:32]=as.numeric(as.character(d13ST[,7:32]))
  d13STVAL <- d13ST%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed5)%>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than\nHigh School",
                                    "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or\nProfessional Degree")))%>%
    mutate(geoname=stri_replace_all_charclass(geoname, "\\p{WHITE_SPACE}", ""))
  
  
  # state Education MOE
  d13STm <- codemog_api(data="b15003_moe",db=ACS,geonum=paste("1",state,sep=""),meta="no")
  d13STm[,7:32]=as.numeric(as.character(d13STm[,7:32]))
  
  #Calculating the summary MOE
  d13STMOE <- d13STm  %>%
    mutate(ed1=sqrt(b15003_moe002^2+b15003_moe003^2+b15003_moe004^2+b15003_moe005^2+b15003_moe006^2+b15003_moe007^2+
                      b15003_moe008^2+b15003_moe009^2+b15003_moe010^2+b15003_moe011^2+b15003_moe012^2+b15003_moe013^2+
                      b15003_moe014^2+b15003_moe015^2+b15003_moe016^2),
           ed2=sqrt(b15003_moe017^2+b15003_moe018^2),
           ed3=sqrt(b15003_moe019^2+b15003_moe020^2+b15003_moe021^2),
           ed4=b15003_moe022,
           ed5=sqrt(b15003_moe023^2+b15003_moe024^2+b15003_moe025^2)) %>%
    select(geoname:geonum,ed1:ed5) %>%
    gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
    mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                           labels=c("Less than\nHigh School",
                                    "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or\nProfessional Degree"))) 
  
  #Preparing data
  names(d13STMOE)[9] <- "MOE"
  d13STVAL2 <- d13STVAL[,c(1,8,10,9)]
  d13STMOE2 <- d13STMOE[,c(8,9)]
  
  d13STF <- merge(d13STVAL2,d13STMOE2,by="EdLevel")
  f.d13STFin <- d13STF %>%
    mutate(s_propVAL = value/sum(value),
           s_propMOE = MOE/sum(value))
  
  f.d13STFin$s_ciLOW  <- f.d13STFin$s_propVAL - f.d13STFin$s_propMOE
  f.d13STFin$s_ciHIGH <- f.d13STFin$s_propVAL + f.d13STFin$s_propMOE
  f.d13STFin$s_pctVAL <- percent(f.d13STFin$s_propVAL *100)
  f.d13STFin$s_pctMOE <- percent(f.d13STFin$s_propMOE *100)
  f.d13STFin$s_pctLOW <- percent(f.d13STFin$s_ciLOW *100)
  f.d13STFin$s_pctHIGH <- percent(f.d13STFin$s_ciHIGH *100)
  
  
  f.d13STFinM <- f.d13STFin[, c(3,2,6,8,9)]
  names(f.d13STFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")
  f.d13STFinM$geoname <- "Colorado"
  
  #place Education Value
  if(nchar(placefips) !=0) {
    d13pl <- codemog_api(data="b15003",db=ACS,geonum=paste("1",state , placefips,sep=""),meta="no")
    d13pl[,7:32]=as.numeric(as.character(d13pl[,7:32]))
    d13plVAL <- d13pl%>%
      mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
               b15003012+b15003013+b15003014+b15003015+b15003016,
             ed2=b15003017+b15003018,
             ed3=b15003019+b15003020+b15003021,
             ed4=b15003022,
             ed5=b15003023+b15003024+b15003025) %>%
      select(geoname:geonum,ed1:ed5)%>%
      gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
      mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                          "ed5"),
                             labels=c("Less than\nHigh School",
                                      "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                      "Graduate or\nProfessional Degree")))
    
    
    # Place Education MOE
    d13plm <- codemog_api(data="b15003_moe",db=ACS,geonum=paste("1",state , placefips,sep=""),meta="no")
    d13plm[,7:32]=as.numeric(as.character(d13plm[,7:32]))
    
    #Calculating the summary MOE
    d13plMOE <- d13plm %>%
      mutate(ed1=sqrt(b15003_moe002^2+b15003_moe003^2+b15003_moe004^2+b15003_moe005^2+b15003_moe006^2+b15003_moe007^2+
                        b15003_moe008^2+b15003_moe009^2+b15003_moe010^2+b15003_moe011^2+b15003_moe012^2+b15003_moe013^2+
                        b15003_moe014^2+b15003_moe015^2+b15003_moe016^2),
             ed2=sqrt(b15003_moe017^2+b15003_moe018^2),
             ed3=sqrt(b15003_moe019^2+b15003_moe020^2+b15003_moe021^2),
             ed4=b15003_moe022,
             ed5=sqrt(b15003_moe023^2+b15003_moe024^2+b15003_moe025^2)) %>%
      select(geoname:geonum,ed1:ed5)%>%
      gather(EdLevel, value, ed1:ed5, factor_key=TRUE)%>%  #Needed to change this part of the call
      mutate(educcat=ordered(as.factor(EdLevel), levels=c("ed1", "ed2", "ed3", "ed4",
                                                          "ed5"),
                             labels=c("Less than\nHigh School",
                                      "High School Graduate\n(or GED)","Some College or\nAssociate's Degree", "Bachelor's Degree",
                                      "Graduate or\nProfessional Degree")))
    
    #Preparing data
    names(d13plMOE)[9] <- "MOE"
    d13plVAL2 <- d13plVAL[,c(1,8,10,9)]
    d13plMOE2 <- d13plMOE[,c(8,9)]
    
    d13plF <- merge(d13plVAL2,d13plMOE2,by="EdLevel")
    f.d13plFin <- d13plF %>%
      mutate(p_propVAL = value/sum(value),
             p_propMOE = MOE/sum(value))
    
    f.d13plFin$p_ciLOW  <- f.d13plFin$p_propVAL - f.d13plFin$p_propMOE
    f.d13plFin$p_ciHIGH <- f.d13plFin$p_propVAL + f.d13plFin$p_propMOE
    f.d13plFin$p_pctVAL <- percent(f.d13plFin$p_propVAL *100)
    f.d13plFin$p_pctMOE <- percent(f.d13plFin$p_propMOE *100)
    f.d13plFin$p_pctLOW <- percent(f.d13plFin$p_ciLOW *100)
    f.d13plFin$p_pctHIGH <- percent(f.d13plFin$p_ciHIGH *100)
    
    f.d13plFinM <- f.d13plFin[, c(3,2,6,8,9)]
    names(f.d13plFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")
    f.d13plFinM$geoname <- placename
  }
  
  
  #Preparing Plot dataset
  if(nchar(placefips) == 0) {
    d <- rbind(f.d13ctyFinM,f.d13STFinM)
    d$geoname <- factor(d$geoname, levels=c(ctyname, "Colorado"))
    subTitle <- ctyname  #The is the county Name...
  } else {
    d <- rbind(f.d13plFinM,f.d13ctyFinM)
    d$geoname <- factor(d$geoname, levels=c(placename, ctyname))
    subTitle <- placename #The is the county Name...
  }
  
  
  d$prop <- d$prop * 100
  d$propLOW <- d$propLOW * 100
  d$propHIGH <- d$propHIGH * 100
  
  d$Education_Cat <- factor(d$Education_Cat, levels=c("Less than\nHigh School",
                                                      "High School Graduate\n(or GED)",
                                                      "Some College or\nAssociate's Degree", "Bachelor's Degree",
                                                      "Graduate or\nProfessional Degree"))
  
  # Preparing Plot
  pltTitle <- "Educational Attainment,\nPersons Age 25 and Older "
  xTitle <- "Educational Attainment"
  
  axs <- setAxis(d$propHIGH)
  minAxs <- min(d$propLOW)
  minAxs <- ifelse(minAxs > 0,0,-10)  
  
  
  p=ggplot(d, aes(x=Education_Cat, y=prop, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+
    geom_errorbar(aes(ymin=propLOW, ymax=propHIGH),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0, size=1.05) +
    scale_y_continuous(limits=c(minAxs, axs$maxBrk), breaks=axs$yBrk, label=percent, expand = c(0, 0))+
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("ACS",ACS),
         x = xTitle,
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=12),
          legend.position= "bottom")
  
  
  # Prepating output data set
  if(nchar(placefips) == 0) {
    ed_county <- f.d13ctyFin[, c(3,6,7,10:13)]
    ed_state <- f.d13STFin[, c(3,6,7,10:13)]
    f.dwide <- merge(ed_county,ed_state,by="educcat")
    
    #calcualting Statistical Test
    #Calculating the statistical test
    f.dwide$ZScore <- (abs(f.dwide$c_propVAL - f.dwide$s_propVAL)/
                         sqrt((f.dwide$c_propMOE^2) + (f.dwide$s_propMOE^2)))
    f.dwide$Sig_Diff <- ifelse(f.dwide$ZScore < 1,"No","Yes")
    f.dwide$Sig_Diff <- ifelse(is.na(f.dwide$Sig_Diff)," ",f.dwide$Sig_Diff)
    
    # Preparing Final File
    f.dwideo <-  f.dwide[,c(1,4:7,10:13,15)]
    
    names(f.dwideo) <- c("Education_Cat",paste0("Percentage: ",ctyname), paste0("Margin of Error: ",ctyname),
                         paste0("Lower 90% Conf Int: ",ctyname),paste0("Upper 90% Conf Int: ",ctyname),
                         "Percentage: Colorado", "Margin of Error: Colorado",
                         "Lower 90% Conf Int: Colorado","Upper 90% Conf Int: Colorado","Significant Difference")
    
    f.dwideo$Education_Cat <- gsub("\\n"," ",f.dwideo$Education_Cat)
    
    f.dwideo <- f.dwideo[c(4,3,5,1,2),]
  } else {
    ed_place <- f.d13plFin[, c(3,6,7,10:13)]
    ed_county <- f.d13ctyFin[, c(3,6,7,10:13)]
    f.dwide <- merge(ed_place,ed_county,by="educcat")
    
    #calcualting Statistical Test
    #Calculating the statistical test
    f.dwide$ZScore <- (abs(f.dwide$p_propVAL - f.dwide$c_propVAL)/
                         sqrt((f.dwide$p_propMOE^2) + (f.dwide$c_propMOE^2)))
    f.dwide$Sig_Diff <- ifelse(f.dwide$ZScore < 1,"No","Yes")
    f.dwide$Sig_Diff <- ifelse(is.na(f.dwide$Sig_Diff)," ",f.dwide$Sig_Diff)
    
    # Preparing Final File
    f.dwideo <-  f.dwide[,c(1,4:7,10:13,15)]
    
    names(f.dwideo) <- c("Education_Cat",paste0("Percentage: ",placename), paste0("Margin of Error: ",placename),
                         paste0("Lower 90% Conf Int: ",placename),paste0("Upper 90% Conf Int: ",placename),
                         paste0("Percentage: ", ctyname), 
                         paste0("Margin of Error: ",ctyname),
                         paste0("Lower 90% Conf Int: ",ctyname),
                         paste0("Upper 90% Conf Int: ",ctyname),
                         "Significant Difference")
    
    f.dwideo$Education_Cat <- gsub("\\n"," ",f.dwideo$Education_Cat)
    
    f.dwideo <- f.dwideo[c(4,3,5,1,2),]
    
  }
  
  
  #bind list
  outList <- list("plot"= p, "data" =  f.dwideo)
  
  return(outList)
}