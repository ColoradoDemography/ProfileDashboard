#' educPRO Creates a Chart comparing educational attainment of two areas
#'
#' Modified from ms_ed in codemogProfile AB 12/2017
#' Uses the codemog_api function to access ACS data (defaults to 13-5yr) to create a ggplot2 chart for
#' use in profiles.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param fips2 is the second area to be compared Defaults to Blank
#' @param state2 is the state of the second place to be compared, is set to call up CO since fips2 is blank Defaults to 08
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic aand data file
#' @export

educPRO <- function(fips, ctyname, state="08", fips2="", state2="08", ACS, base=10){

  #Place Education Value
  d13p <- codemog_api(data="b15003",db=ACS,geonum=paste("1",state , fips,sep=""),meta="no")
  d13p[,7:32]=as.numeric(as.character(d13p[,7:32]))
  d13pVAL <- d13p%>%
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
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))

  # Place Education MOE
  d13pm <- codemog_api(data="b15003_moe",db=ACS,geonum=paste("1",state , fips,sep=""),meta="no")
  d13pm[,7:32]=as.numeric(as.character(d13pm[,7:32]))

  #Calculating the summary MOE
  d13pMOE <- d13pm %>%
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
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))

  #Preparing data
  names(d13pMOE)[9] <- "MOE"
  d13pVAL2 <- d13pVAL[,c(1,8,10,9)]
  d13pMOE2 <- d13pMOE[,c(8,9)]

  d13pF <- merge(d13pVAL2,d13pMOE2,by="EdLevel")
  f.d13pFin <- d13pF %>%
    mutate(p_propVAL = value/sum(value),
           p_propMOE = MOE/sum(value))

  f.d13pFin$p_ciLOW  <- f.d13pFin$p_propVAL - f.d13pFin$p_propMOE
  f.d13pFin$p_ciHIGH <- f.d13pFin$p_propVAL + f.d13pFin$p_propMOE
  f.d13pFin$p_pctVAL <- percent(f.d13pFin$p_propVAL *100)
  f.d13pFin$p_pctMOE <- percent(f.d13pFin$p_propMOE *100)
  f.d13pFin$p_pctLOW <- percent(f.d13pFin$p_ciLOW *100)
  f.d13pFin$p_pctHIGH <- percent(f.d13pFin$p_ciHIGH *100)

  f.d13pFinM <- f.d13pFin[, c(3,2,6,8,9)]
  names(f.d13pFinM)[2] <- ctyname
  names(f.d13pFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")


  #State Education Values
  d13c <- codemog_api(data="b15003",db=ACS,geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13c[,7:32]=as.numeric(as.character(d13c[,7:32]))
  d13cVAL <- d13c%>%
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
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    mutate(geoname=stri_replace_all_charclass(geoname, "\\p{WHITE_SPACE}", ""))


  # state Education MOE
  d13cm <- codemog_api(data="b15003_moe",db=ACS,geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13cm[,7:32]=as.numeric(as.character(d13cm[,7:32]))

  #Calculating the summary MOE
  d13cMOE <- d13cm%>%
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
                           labels=c("Less than High School",
                                    "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                    "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))

  #Preparing data
  names(d13cMOE)[9] <- "MOE"
  d13cVAL2 <- d13cVAL[,c(1,8,10,9)]
  d13cMOE2 <- d13cMOE[,c(8,9)]

  d13cF <- merge(d13cVAL2,d13cMOE2,by="EdLevel")
  f.d13cFin <- d13cF %>%
    mutate(s_propVAL = value/sum(value),
           s_propMOE = MOE/sum(value))

  f.d13cFin$s_ciLOW  <- f.d13cFin$s_propVAL - f.d13cFin$s_propMOE
  f.d13cFin$s_ciHIGH <- f.d13cFin$s_propVAL + f.d13cFin$s_propMOE
  f.d13cFin$s_pctVAL <- percent(f.d13cFin$s_propVAL *100)
  f.d13cFin$s_pctMOE <- percent(f.d13cFin$s_propMOE *100)
  f.d13cFin$s_pctLOW <- percent(f.d13cFin$s_ciLOW *100)
  f.d13cFin$s_pctHIGH <- percent(f.d13cFin$s_ciHIGH *100)

  f.d13cFinM <- f.d13cFin[, c(3,2,6,8,9)]
  names(f.d13cFinM) <- c("Education_Cat","geoname","prop","propLOW","propHIGH")


  #Preparing Plot dataset
  d <- rbind(f.d13pFinM,f.d13cFinM)

  d$Education_Cat <- factor(d$Education_Cat, levels=c("Less than High School",
                                                      "High School Graduate \n(or GED)",
                                                      "Some College or \nAssociate's Degree", "Bachelor's Degree",
                                                      "Graduate or \nProfessional Degree"))

  # Preparing Plot
  d$geoname <- factor(d$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Educational Attaiment,\nPersons Age 25 and Older "
  subTitle <- ctyname  #The is the county Name...
  xTitle <- "Educational Attainment"

  p=ggplot(d, aes(x=Education_Cat, y=prop, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+
    geom_errorbar(aes(ymin=propLOW, ymax=propHIGH),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    scale_y_continuous(label=percent, expand = c(0, 0))+
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=0))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("ACS",ACS),
         x = xTitle,
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")


  # Prepating output data set
  ed_place <- f.d13pFin[, c(3,6,7,10:13)]

  ed_state <- f.d13cFin[, c(3,6,7,10:13)]


  f.dwide <- merge(ed_place,ed_state,by="educcat")



  #calcualting Statistical Test
  #Calculating the statistical test
  f.dwide$ZScore <- (abs(f.dwide$p_propVAL - f.dwide$s_propVAL)/
                       sqrt((f.dwide$p_propMOE^2) + (f.dwide$s_propMOE^2)))
  f.dwide$Sig_Diff <- ifelse(f.dwide$ZScore < 1,"No","Yes")
  f.dwide$Sig_Diff <- ifelse(is.na(f.dwide$Sig_Diff)," ",f.dwide$Sig_Diff)


  # Preparing Final File
  f.dwideo <-  f.dwide[,c(1,4:7,10:13,15)]

  names(f.dwideo) <- c("Education_Cat",paste0("Percentage: ",ctyname), paste0("Margin of Error: ",ctyname),
                       paste0("Lower 90% Conf Int: ",ctyname),paste0("Upper 90% Conf Int: ",ctyname),
                       "Percentage: Colorado", "Margin of Error: Colorado",
                       "Lower 90% Conf Int: Colorado","Upper 90% Conf Int: Colorado","Significant Difference")

  f.dwideo$Education_Cat <- gsub("\\n","",f.dwideo$Education_Cat)

  f.dwideo <- f.dwideo[c(4,3,5,1,2),]


  #bind list
  outList <- list("plot"= p, "data" =  f.dwideo)

  return(outList)
}
