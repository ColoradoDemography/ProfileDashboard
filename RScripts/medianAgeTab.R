#' medianAgeTab Creates table showing the Median Age by Gender
#' for a selected place and county or for a copunty and for the state
#'
#' @param ctyfips The County FIPS number (without leading Zeros)
#' @param ctyname a string identiying the county name
#' @param placefips The County FIPS number (without leading Zeros)
#' @param placename a string identiying the county name
#' @param ACS a string identifying the input dataset eg: "acs1115"
#' @param state the State FIPS code, defaluts to "08" for Colorado.
#' @param oType type of output, html or latex
#' @return a formatted table and dataset
#' @export

medianAgeTab <- function(ctyfips, ctyname, placefips, placename, ACS, oType, state="08"){

  #County  Age
  medAgecty <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
  medAgectyMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")

  medAgecty2 <- gather(medAgecty[1,8:10])
  medAgecty2$key <- c("Total","Male","Female")
  names(medAgecty2)[2] <- "MedAge_c"

  medAgecty2MOE <- gather(medAgectyMOE[1,8:10])
  medAgecty2MOE$key <- c("Total","Male","Female")
  names(medAgecty2MOE)[2] <- "MOE_c"

  f.ctyAge <- merge(medAgecty2, medAgecty2MOE, by = "key")

  #State Age
  medAgeST  <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  medAgeSTMOE  <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state,  sep=""), meta="no")

  medAgeST2 <- gather(medAgeST[1,8:10])
  medAgeST2$key <- c("Total","Male","Female")
  names(medAgeST2)[2] <- "MedAge_s"

  medAgeST2MOE <- gather(medAgeSTMOE[1,8:10])
  medAgeST2MOE$key <- c("Total","Male","Female")
  names(medAgeST2MOE)[2] <- "MOE_s"

  f.stateAge <- merge(medAgeST2, medAgeST2MOE, by = "key")
  
  if(nchar(placefips) != 0) {
    #place  Age
    medAgeplace <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    medAgeplaceMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    
    medAgeplace2 <- gather(medAgeplace[1,8:10])
    medAgeplace2$key <- c("Total","Male","Female")
    names(medAgeplace2)[2] <- "MedAge_p"
    
    medAgeplace2MOE <- gather(medAgeplaceMOE[1,8:10])
    medAgeplace2MOE$key <- c("Total","Male","Female")
    names(medAgeplace2MOE)[2] <- "MOE_p"
    
    f.placeAge <- merge(medAgeplace2, medAgeplace2MOE, by = "key")
  }

  #Creating Copmbined table and Calcualting tests
  if(nchar(placefips) == 0) {
    f.ageTab <- merge(f.ctyAge, f.stateAge, by = "key")
    #Calculating significant differences
    f.ageTab$MedAge_c <- as.numeric(f.ageTab$MedAge_c)
    f.ageTab$MOE_c <- as.numeric(f.ageTab$MOE_c)
    f.ageTab$MedAge_s <- as.numeric(f.ageTab$MedAge_s)
    f.ageTab$MOE_s <- as.numeric(f.ageTab$MOE_s)
    
    f.ageTab$ZScore <- (abs(f.ageTab$MedAge_c - f.ageTab$MedAge_s)/
                          sqrt((f.ageTab$MOE_c^2) + (f.ageTab$MOE_s^2)))
    f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
    f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_c < f.ageTab$MedAge_s,"Younger","Older"),"")
    
    m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
  } else {
    f.ageTab <- merge(f.placeAge, f.ctyAge, by = "key")
    #Calculating significant differences
    f.ageTab$MedAge_p <- as.numeric(f.ageTab$MedAge_p)
    f.ageTab$MOE_p <- as.numeric(f.ageTab$MOE_p)
    f.ageTab$MedAge_c <- as.numeric(f.ageTab$MedAge_c)
    f.ageTab$MOE_c <- as.numeric(f.ageTab$MOE_c)
    
    f.ageTab$ZScore <- (abs(f.ageTab$MedAge_p - f.ageTab$MedAge_c)/
                          sqrt((f.ageTab$MOE_p^2) + (f.ageTab$MOE_c^2)))
    f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
    f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_p < f.ageTab$MedAge_c,"Younger","Older"),"")
    
    m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
    
  }

  

  #Column Names
  if(nchar(placename) == 0)  {
    names_spaced <- c("Gender","Median Age","Margin of Error","Median Age","Margin of Error","Signficant Difference?","Difference from State")
  } else {
    names_spaced <- c("Gender","Median Age","Margin of Error","Median Age","Margin of Error","Signficant Difference?","Difference from County")
  }
  #Span Header
  if(nchar(placefips) == 0) {
    # create vector with colspan
    tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 2)
    # set vector names
    names(tblHead) <- c(" ", ctyname,"Colorado"," ")
  } else {
    # create vector with colspan
    tblHead <- c(" " = 1, placename = 2, ctyname  = 2, " " = 2)
    # set vector names
    names(tblHead) <- c(" ", placename,ctyname," ")
  }
  
  
  if(oType == "html") {
  age_t <- m.ageTab %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrrrr',
          caption="Median Age by Gender  Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "0.5in") %>%
    column_spec(2, width = "0.75in") %>%
    column_spec(3, width = "0.75in") %>%
    column_spec(4, width = "0.75in") %>%
    column_spec(5, width = "0.75in") %>%
    column_spec(6, width = "1in") %>%
    column_spec(7, width = "0.75in") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(captionSrc("ACS",ACS))

  #preparint Output data
  f.ageTab2 <- f.ageTab[,c(1:5,7,8)]
  
if(nchar(placename) == 0)  {
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",ctyname), paste0("Margin of Error: ",ctyname),
                        "Median Age: Colorado", "Margin of Error: Colorado", "Sig. Difference","Difference from State")
} else {
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",placename), paste0("Margin of Error: ",placename),
                        paste0("Median Age: ",ctyname), paste0("Margin of Error: ",ctyname), "Sig. Difference","Difference from County")
}
  

  outList <- list("table" = age_t, "data" = f.ageTab2)
  return(outList)
  }

  if(oType == "latex") {

  tabOut <- m.ageTab %>% kable(digits=1,
                  row.names=FALSE,
                  align='lrrrrrr',
                  caption="Median Age by Gender Comparison",  format="latex", booktabs=TRUE,
                  col.names = names_spaced)  %>%
    kable_styling(latex_options="HOLD_position") %>%
    column_spec(1, width = "0.5in") %>%
    column_spec(2, width = "0.75in") %>%
    column_spec(3, width = "0.75in") %>%
    column_spec(4, width = "0.75in") %>%
    column_spec(5, width = "0.75in") %>%
    column_spec(6, width = "1in") %>%
    column_spec(7, width = "0.75in") %>%
    add_header_above(header=tblHead) %>%
    add_footnote(captionSrc("ACS",ACS))

  PlAge <- as.numeric(m.ageTab[3,2])
  StAge <- as.numeric(m.ageTab[3,4])
  dval <- ifelse(PlAge > StAge, PlAge - StAge,
          ifelse(PlAge == StAge,0,StAge - PlAge))
  sDiffa <- m.ageTab[3,6]
  diffDira <- tolower(m.ageTab[3,7])
  sDiffm <- m.ageTab[2,6]
  diffDirm <- tolower(m.ageTab[2,7])
  sDifff <- m.ageTab[1,6]
  diffDirf <- tolower(m.ageTab[1,7])

  if(nchar(placefips) == 0) {
  if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "Yes") {
    medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
    medText <- paste0(medText, " Women in ",ctyname," are significantly ",diffDirf, " than women in the state and men in ",ctyname," are significantly ",diffDirm," than men in the state.")
  }

  if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "Yes") {
    medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
    medText <- paste0(medText, " Women in ",ctyname," are significantly ", diffDirf," than women in the state but men are not sigificnatly older or younger than men in the state.")
  }

  if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "No") {
    medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
    medText <- paste0(medText, " Women are not significantly older or younger than women in the state but men in ",ctyname," are significantly ",diffDirm," than men in the state.")
  }

  if(sDiffa == "No" & sDiffm == "No" && sDifff == "No") {
    medText <- paste0("The median age of ",ctyname," is not significantly different than population of the state.")
  }
  } else {
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "Yes") {
      medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the county.")
      medText <- paste0(medText, " Women in ",ctyname," are significantly ",diffDirf, " than women in the county and men in ",ctyname," are significantly ",diffDirm," than men in the county.")
    }
    
    if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "Yes") {
      medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the county.")
      medText <- paste0(medText, " Women in ",ctyname," are significantly ", diffDirf," than women in the county but men are not sigificnatly older or younger than men in the county.")
    }
    
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "No") {
      medText <- paste0("The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the county.")
      medText <- paste0(medText, " Women are not significantly older or younger than women in the county but men in ",ctyname," are significantly ",diffDirm," than men in the county.")
    }
    
    if(sDiffa == "No" & sDiffm == "No" && sDifff == "No") {
      medText <- paste0("The median age of ",ctyname," is not significantly different than population of the county.")
    }
   }
  outList <- list("table" = tabOut, "text" = medText)
  return(outList)

  }
}
