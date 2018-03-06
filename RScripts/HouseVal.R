#' HouseVal Table Showing the Median House value for Owner-occupied housing,
#'     the Median Gross Rent, and Median Costs as a Percentage of Income for
#'     Owners and Renters for a place and the State of Colorado
#'
#' @param ctyfips is the fips code for the selected county
#' @param ctyname is the name of the selected county
#' @param placefips is the fips code for the selected municipality
#' @param placename is the name of the selected municipality
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param oType output type html table or latex table
#' @return kable formatted  table and data file
#' @export
#'

HouseVal <- function(ctyfips,ctyname, placefips, placename, ACS, oType, state="08"){

  # Raw County data Owners
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Median Value
  f.b25095 <- codemog_api(data="b25095", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # costs as % of Income

  # Raw County data Renters
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Median Value
  f.b25074 <- codemog_api(data="b25074", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # costs as % of Income

  f.ACSCTY <- cbind(f.b25077[,c(1,8)],f.b25095[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                    f.b25064[,c(1,8)],f.b25074[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])

  f.ACSCTY <- f.ACSCTY[,c(1,2,4:36,38,40:68)]
  f.ACSCTY[,2:65] <-as.numeric(as.character(f.ACSCTY[,2:65]))

   tot_OO <- as.numeric(f.ACSCTY$b25095001)
   tot_RT <- as.numeric(f.ACSCTY$b25074001)

  f.ACSCTY <- f.ACSCTY %>% mutate(
    Med_Val = b25077001,
    OO_3049 = b25095006 + b25095007 + b25095008 + 
              b25095015 + b25095016 + b25095017 + 
              b25095024 + b25095025 + b25095026 + 
              b25095033 + b25095034 + b25095035 + 
              b25095042 + b25095043 + b25095044 + 
              b25095051 + b25095052 + b25095053 + 
              b25095060 + b25095061 + b25095062 + 
              b25095069 + b25095070 + b25095071,
    OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
       PROP_OO_50 = OO_50/tot_OO,
      PCT_OO_50 = percent(PROP_OO_50*100),
      Med_Rent = b25064001,
      RT_3049 = b25074006 + b25074007 + b25074008 + 
      b25074015 + b25074016 + b25074017 + 
      b25074024 + b25074025 + b25074026 + 
      b25074033 + b25074034 + b25074035 + 
      b25074042 + b25074043 + b25074044 + 
      b25074051 + b25074052 + b25074053 + 
      b25074060 + b25074061 + b25074062,
    RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )

  f.ACSCTY <- f.ACSCTY[,c(1,66,69:72,73,76:79)]

  f.ACSCTYL <- as.data.frame(t(f.ACSCTY))
  names(f.ACSCTYL)[1] <- "CTY_VAL"
  f.ACSCTYL <- rownames_to_column( f.ACSCTYL,"value")

 
  # Raw County data Owners MOE
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Median Value
  f.b25095_moe <- codemog_api(data="b25095_moe", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # costs as % of Income
  
  # Raw County data Renters MOE
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Median Value
  f.b25074_moe <- codemog_api(data="b25074_moe", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # costs as % of Income
  
  f.ACSCTY_moe <- cbind(f.b25077_moe[,c(1,8)],f.b25095_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                    f.b25064_moe[,c(1,8)],f.b25074_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])
  
  f.ACSCTY_moe <- f.ACSCTY_moe[,c(1,2,4:36,38,40:68)]
  f.ACSCTY_moe[,2:65] <-as.numeric(as.character(f.ACSCTY_moe[,2:65]))
  
  
  f.ACSCTY_moe <- f.ACSCTY_moe %>% mutate(  
    Med_Val = b25095_moe001,
    OO_3049 = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
      b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
      b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
      b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
      b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
      b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
      b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
      b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
    OO_50 = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                 b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/tot_OO,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25074_moe001,
    RT_3049 = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
      b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
      b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
      b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
      b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
      b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
      b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
    RT_50 = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                 b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )
  f.ACSCTY_moe <- f.ACSCTY_moe[,c(1,66,69:72,73,76:79)]
 
  f.ACSCTYL_moe <- as.data.frame(t(f.ACSCTY_moe))
  names(f.ACSCTYL_moe)[1] <- "Cty_MOE"
  f.ACSCTYL_moe <- rownames_to_column(f.ACSCTYL_moe,"value")

  f.ACSCTY_Fin <- merge(f.ACSCTYL,f.ACSCTYL_moe,by ="value")

  

  # Assembling State Data
  # Raw State data Owners
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25095 <- codemog_api(data="b25095", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income
  
  # Raw Place data Renters
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25074 <- codemog_api(data="b25074", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income
  
  f.ACSST <- cbind(f.b25077[,c(1,8)],f.b25095[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                    f.b25064[,c(1,8)],f.b25074[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])
  
  f.ACSST <- f.ACSST[,c(1,2,4:36,38,40:68)]
  f.ACSST[,2:65] <-as.numeric(as.character(f.ACSST[,2:65]))
  
  tot_OO <- as.numeric(f.ACSST$b25095001)
  tot_RT <- as.numeric(f.ACSST$b25074001)
  
  f.ACSST <- f.ACSST %>% mutate(
    Med_Val = b25077001,
    OO_3049 = b25095006 + b25095007 + b25095008 + 
      b25095015 + b25095016 + b25095017 + 
      b25095024 + b25095025 + b25095026 + 
      b25095033 + b25095034 + b25095035 + 
      b25095042 + b25095043 + b25095044 + 
      b25095051 + b25095052 + b25095053 + 
      b25095060 + b25095061 + b25095062 + 
      b25095069 + b25095070 + b25095071,
    OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/tot_OO,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25064001,
    RT_3049 = b25074006 + b25074007 + b25074008 + 
      b25074015 + b25074016 + b25074017 + 
      b25074024 + b25074025 + b25074026 + 
      b25074033 + b25074034 + b25074035 + 
      b25074042 + b25074043 + b25074044 + 
      b25074051 + b25074052 + b25074053 + 
      b25074060 + b25074061 + b25074062,
    RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )
  f.ACSST <- f.ACSST[,c(1,66,69:72,73,76:79)]
  
  f.ACSSTL <- as.data.frame(t(f.ACSST))
  names(f.ACSSTL)[1] <- "ST_VAL"
  f.ACSSTL <- rownames_to_column( f.ACSSTL,"value")
  
  
  # Raw State data Owners MOE
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25095_moe <- codemog_api(data="b25095_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income
  
  # Raw State data Renters MOE
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state),meta="no") # Median Value
  f.b25074_moe <- codemog_api(data="b25074_moe", db=ACS, geonum=paste0("1", state),meta="no") # costs as % of Income
  
  f.ACSST_moe <- cbind(f.b25077_moe[,c(1,8)],f.b25095_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                        f.b25064_moe[,c(1,8)],f.b25074_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])
  
  f.ACSST_moe <- f.ACSST_moe[,c(1,2,4:36,38,40:68)]
  f.ACSST_moe[,2:65] <-as.numeric(as.character(f.ACSST_moe[,2:65]))
  
  
  f.ACSST_moe <- f.ACSST_moe %>% mutate(  
    Med_Val = b25095_moe001,
    OO_3049 = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
                     b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
                     b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
                     b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
                     b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
                     b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
                     b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
                     b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
    OO_50 = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                   b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/tot_OO,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25074_moe001,
    RT_3049 = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
                     b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
                     b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
                     b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
                     b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
                     b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
                     b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
    RT_50 = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                   b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )
  f.ACSST_moe <- f.ACSST_moe[,c(1,66,69:72,73,76:79)]
 
  f.ACSSTL_moe <- as.data.frame(t(f.ACSST_moe))
  names(f.ACSSTL_moe)[1] <- "ST_MOE"
  f.ACSSTL_moe <- rownames_to_column(f.ACSSTL_moe,"value")
  
  f.ACSST_Fin <- merge(f.ACSSTL,f.ACSSTL_moe,by ="value")


  #Place Data
  if(nchar(placefips) !=0) {
  # Raw Place data Owners
  f.b25077 <- codemog_api(data="b25077", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Median Value
  f.b25095 <- codemog_api(data="b25095", db=ACS, geonum=paste0("1", state, placefips),meta="no") # costs as % of Income
  
  # Raw Place data Renters
  f.b25064 <- codemog_api(data="b25064", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Median Value
  f.b25074 <- codemog_api(data="b25074", db=ACS, geonum=paste0("1", state, placefips),meta="no") # costs as % of Income
  
  f.ACSPL <- cbind(f.b25077[,c(1,8)],f.b25095[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                    f.b25064[,c(1,8)],f.b25074[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])
  
  f.ACSPL <- f.ACSPL[,c(1,2,4:36,38,40:68)]
  f.ACSPL[,2:65] <-as.numeric(as.character(f.ACSPL[,2:65]))
  
  tot_OO <- as.numeric(f.ACSPL$b25095001)
  tot_RT <- as.numeric(f.ACSPL$b25074001)
  
  f.ACSPL <- f.ACSPL %>% mutate(
    Med_Val = b25077001,
    OO_3049 = b25095006 + b25095007 + b25095008 + 
      b25095015 + b25095016 + b25095017 + 
      b25095024 + b25095025 + b25095026 + 
      b25095033 + b25095034 + b25095035 + 
      b25095042 + b25095043 + b25095044 + 
      b25095051 + b25095052 + b25095053 + 
      b25095060 + b25095061 + b25095062 + 
      b25095069 + b25095070 + b25095071,
    OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/tot_OO,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25064001,
    RT_3049 = b25074006 + b25074007 + b25074008 + 
      b25074015 + b25074016 + b25074017 + 
      b25074024 + b25074025 + b25074026 + 
      b25074033 + b25074034 + b25074035 + 
      b25074042 + b25074043 + b25074044 + 
      b25074051 + b25074052 + b25074053 + 
      b25074060 + b25074061 + b25074062,
    RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )
  f.ACSPL <- f.ACSPL[,c(1,66,69:72,73,76:79)]
  
  f.ACSPLL <- as.data.frame(t(f.ACSPL))
  names(f.ACSPLL)[1] <- "PL_VAL"
  f.ACSPLL <- rownames_to_column( f.ACSPLL,"value")
  
  
  # Raw Place data Owners MOE
  f.b25077_moe <- codemog_api(data="b25077_moe", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Median Value
  f.b25095_moe <- codemog_api(data="b25095_moe", db=ACS, geonum=paste0("1", state, placefips),meta="no") # costs as % of Income
  
  # Raw Place data Renters MOE
  f.b25064_moe <- codemog_api(data="b25064_moe", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Median Value
  f.b25074_moe <- codemog_api(data="b25074_moe", db=ACS, geonum=paste0("1", state, placefips),meta="no") # costs as % of Income
  
  f.ACSPL_moe <- cbind(f.b25077_moe[,c(1,8)],f.b25095_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70,76:79)],
                        f.b25064_moe[,c(1,8)],f.b25074_moe[,c(1,8,13:16,22:25,31:34,40:43,49:52,58:61,67:70)])
  
  f.ACSPL_moe <- f.ACSPL_moe[,c(1,2,4:36,38,40:68)]
  f.ACSPL_moe[,2:65] <-as.numeric(as.character(f.ACSPL_moe[,2:65]))
  
  
  f.ACSPL_moe <- f.ACSPL_moe %>% mutate(  
    Med_Val = b25095_moe001,
    OO_3049 = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
                     b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
                     b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
                     b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
                     b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
                     b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
                     b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
                     b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
    OO_50 = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                   b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
    PROP_OO_3049 = OO_3049/tot_OO,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/tot_OO,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25074_moe001,
    RT_3049 = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
                     b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
                     b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
                     b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
                     b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
                     b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
                     b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
    RT_50 = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                   b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
    PROP_RT_3049 = RT_3049/tot_RT,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/tot_RT,
    PCT_RT_50 = percent(PROP_RT_50*100)
  )
  f.ACSPL_moe <- f.ACSPL_moe[,c(1,66,69:72,73,76:79)]
  
  f.ACSPLL_moe <- as.data.frame(t(f.ACSPL_moe))
  names(f.ACSPLL_moe)[1] <- "PL_MOE"
  f.ACSPLL_moe <- rownames_to_column(f.ACSPLL_moe,"value")
  
  f.ACSPL_Fin <- merge(f.ACSPLL,f.ACSPLL_moe,by ="value")
}

  # Joining Fles and calculating tests
  if(nchar(placefips) == 0) {
    f.HouseVal <- merge(f.ACSCTY_Fin,f.ACSST_Fin, by="value")

  } else {
    f.HouseVal <- merge(f.ACSPL_Fin,f.ACSCTY_Fin, by="value")
  }
  
  m.HouseVal <- as.matrix(f.HouseVal)
  m.test <- matrix(nrow=11, ncol=2)
  #calculating statistical test
  for(i in c(2,3,8:11)) {
    m.test[i,1] <-abs(as.numeric(m.HouseVal[i,2]) - as.numeric(m.HouseVal[i,4]))/sqrt((as.numeric(m.HouseVal[i,3])^2) + (as.numeric(m.HouseVal[i,5])^2))
    m.test[i,2] <- ifelse(m.test[i,1] < 1,"No","Yes")
  }
  for(i in 4:7) {
    m.test[i,1] = m.test[i+4,1]
    m.test[i,2] = m.test[i+4,2]
  }
  m.HouseVal <- cbind(m.HouseVal,m.test)
  
# Bulding Renter and owner-occupied mataices for table 
m.rental <- m.HouseVal[c(2,6,7),c(1:5,7)]  
m.oocc <- m.HouseVal[c(3:5),c(1:5,7)] 

m.rental[1,1] <- "Median Gross Rent of Rental Housholds (Current Dollars)"
m.rental[2,1] <- "Percentage of Rental Households paying 30-49% of income on housing"
m.rental[3,1] <- "Percentage of Rental Households paying 50% or more of income on housing"

m.oocc[1.1] <- "Median Value of owner-Occupied Households (Current Dollars)"
m.oocc[2,1] <- "Percentage of owner-occupied households paying 30-49% of income on housing"
m.oocc[3,1] <- "Percentage of owner-occupied households paying 50% or more of income on rent"

#formatting values
m.rental[1,2:5] <- paste0("$",formatC(as.numeric(m.rental[1,2:5]),format="f",digits=0,big.mark=","))
m.oocc[1,2:5] <- paste0("$",formatC(as.numeric(m.oocc[1,2:5]),format="f",digits=0,big.mark=","))

#Creating output data set
m.FinTab <- rbind(m.oocc,m.rental)
f.HouseVal_Fin <- as.data.frame(m.FinTab)


if(nchar(placefips) == 0) {
  names(f.HouseVal_Fin) <- c("Variable",paste0("Value: ",ctyname), paste0("Margin of Error: ",ctyname),
                             paste0("Value: Colorado"), paste0("Margin of Error: Colorado"), "Siginficant Difference?")
} else {
  names(f.HouseVal_Fin) <- c("Variable",paste0("Value: ",placename), paste0("Margin of Error: ",placename),
                             paste0("Value: ",ctyname), paste0("Margin of Error: ",ctyname), "Siginficant Difference?")
  
}

  # Setting up table

  #Column Names
  names_spaced <- c("Variable","Value","Margin of Error","Value","Margin of Error","Sig. Diff.?")
  #Span Header

  # create vector with colspan
  if(nchar(placefips) == 0) {
    tblHead1 <- c(" " = 1, ctyname = 2, "Colorado" = 2, " " = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", ctyname, "Colorado", " ")
  }  else {
    tblHead1 <- c(" " = 1, placename = 2, ctyname = 2, " " = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", placename, ctyname, " ")
  }
 

  if(oType == "html") {
    Housing_tab1 <- m.oocc %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrrr',
            caption="Comparative Owner-Occupied Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size=11) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      column_spec(6, width ="0.4in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    Housing_tab2 <- m.rental %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrrr',
            caption="Comparative Rental Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size=11) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      column_spec(6, width ="0.4in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    outList <- list("table0" = Housing_tab1, "tableR" = Housing_tab2, "data" = f.HouseVal_Fin)
    return(outList)
  }

  if(oType == "latex") {

    OOTab <-  kable(m.oocc,
                     col.names = names_spaced,
                     align=c("lrrrrr"),
                     caption="Comparison of Housing Values", row.names=FALSE,
                     format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      row_spec(1, bold = TRUE, italic = TRUE) %>%
      row_spec(4, bold = TRUE, italic = TRUE) %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      column_spec(6, width ="0.4in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))
    
    RTTab <-  kable(m.rental,
                    col.names = names_spaced,
                    align=c("lrrrrr"),
                    caption="Comparison of Housing Values", row.names=FALSE,
                    format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      row_spec(1, bold = TRUE, italic = TRUE) %>%
      row_spec(4, bold = TRUE, italic = TRUE) %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      column_spec(6, width ="0.4in") %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))

    outList <- list("OOTab" = OOTab, "RTTab" = RTTab)
    return(outList)
  }
}
