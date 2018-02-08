#' medianAgeTab Creates table showing the Median Age by Gender
#' for a selected place and for the state
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param state the State FIPS code, defaluts to "08" for Colorado.
#' @param ACS a string identifying the input dataset eg: "acs1115"
#' @param ctyname a string identiying the place name
#' @param oType type of output, html or latex
#' @return a formatted table and dataset
#' @export

medianAgeTab <- function(fips, ACS, ctyname, oType, state="08"){

  #Local place Age
  medAge <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, fips, sep=""), meta="no")
  medAgeMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, fips, sep=""), meta="no")

  medAge2 <- gather(medAge[1,8:10])
  medAge2$key <- c("Total","Male","Female")
  names(medAge2)[2] <- "MedAge_p"

  medAge2MOE <- gather(medAgeMOE[1,8:10])
  medAge2MOE$key <- c("Total","Male","Female")
  names(medAge2MOE)[2] <- "MOE_p"

  f.localAge <- merge(medAge2, medAge2MOE, by = "key")

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

  #Creating Copmbined table

  f.ageTab <- merge(f.localAge, f.stateAge, by = "key")



  #Calculating significant differences
  f.ageTab$MedAge_p <- as.numeric(f.ageTab$MedAge_p)
  f.ageTab$MOE_p <- as.numeric(f.ageTab$MOE_p)
  f.ageTab$MedAge_s <- as.numeric(f.ageTab$MedAge_s)
  f.ageTab$MOE_s <- as.numeric(f.ageTab$MOE_s)

  f.ageTab$ZScore <- (abs(f.ageTab$MedAge_p - f.ageTab$MedAge_s)/
                        sqrt((f.ageTab$MOE_p^2) + (f.ageTab$MOE_p^2)))
  f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
  f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_p < f.ageTab$MedAge_s,"Younger","Older"),"")

  m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
  #Column Names

  names_spaced <- c("Gender","Median Age","Margin of Error","Median Age","Margin of Error","Signficant Difference?","Difference from State")

  #Span Header

  # create vector with colspan
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 2)

  # set vector names
  names(tblHead) <- c(" ", ctyname,"Colorado"," ")
  if(oType == "html") {
  age_t <- m.ageTab %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrrrr',
          caption="Median Age by Gender  Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
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
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",ctyname), paste0("Margin of Error: ",ctyname),
                        "Median Age: Colorado", "Margin of Error: Colorado", "Sig. Difference","Difference from State")

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

  outList <- list("table" = tabOut, "text" = medText)
  return(outList)

  }
}
