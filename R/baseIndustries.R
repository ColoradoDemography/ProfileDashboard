#' baseIndustries outputs the base_industries job plot and data
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param  fips numeric, county-level FIPS code
#' @param  ctyname County Name string, from input$unit
#' @param  curyr the current year
#' @return ggplot2 graphic and data file
#' @export

baseIndustries <- function(fips, ctyname, curyr, oType,base=10){
  #fips is the 3-digit character string

  # creating alternative fips code for Denver MSA
  if(fips %in% c("001", "005", "013", "014", "031", "035", "059")) {
    fips = "500"
    ctyname = "Denver-Boulder MSA"
  }

  jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",fips, "';")



  pw <- {
    "demography"
  }

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "dola",
                   host = "104.197.26.248", port = 5433,
                   user = "codemog", password = pw)
  rm(pw) # removes the password

  # Read data files

  f.jobsBase <- dbGetQuery(con, jobsSQL)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)


  # convert datasets to long

  f.jobsBaseL <- gather(f.jobsBase, industry, jobs, employment:direct_basic_emp)

  # Table data
  f.jobsBaseTab <- f.jobsBaseL[which(f.jobsBaseL$industry %in% c("employment",  "direct_basic_emp", "ib_emp", "wrkr_lrs_emp")),]




  f.jobsBaseL$industry <- if_else(f.jobsBaseL$industry == "agri_emp","Agriculture",
                                  if_else(f.jobsBaseL$industry == "mining_emp","Mining",
                                          if_else(f.jobsBaseL$industry == "manuf_emp","Manufacturing",
                                                  if_else(f.jobsBaseL$industry == "govt_emp","Government",
                                                          if_else(f.jobsBaseL$industry == "regl_serv_emp","Regional Services",
                                                                  if_else(f.jobsBaseL$industry == "tourism_emp","Tourism",
                                                                          if_else(f.jobsBaseL$industry == "other_hhd_emp","Other Household",
                                                                                  if_else(f.jobsBaseL$industry == "retiree_emp","Retiree(s)",
                                                                                          if_else(f.jobsBaseL$industry == "commuter_emp", "Commuter",
                                                                                                  if_else(f.jobsBaseL$industry == "other_inc_emp","Other Income","drop"))))))))))



  # Building Plot Data
  f.jobsBaseFin <- f.jobsBaseL[which(f.jobsBaseL$industry != "drop"),]

  f.jobsBaseFin <-  f.jobsBaseFin %>%
    mutate (prop = jobs/sum(jobs) * 100,
            pct = percent(prop))


  #sorting records
  f.jobsBaseFin <- f.jobsBaseFin[order(-f.jobsBaseFin$prop),]
  f.jobsBaseFin$industry <- factor(f.jobsBaseFin$industry,
                                   levels=f.jobsBaseFin$industry[order(-f.jobsBaseFin$prop)], ordered=TRUE)


  pltTitle <- paste0(curyr," Base Industries (without Indirect)")
  cPallette <- c( "Agriculture"  = "deepskyblue4",
                  "Manufacturing" = 	"deepskyblue3",
                  "Mining" =	"deepskyblue2",
                  "Government" = "darkseagreen4",
                  "Regional Services" = 	"darkseagreen3",
                  "Commuter" = "darkseagreen3",
                  "Tourism" =	"darkseagreen1",
                  "Other Household"= "gold3",
                  "Other Income" =	"gold2",
                  "Retiree(s)" =	"gold1"
  )


  BaseBars <-  f.jobsBaseFin %>%
    ggplot(aes(x=industry, y=prop, fill=industry), color="black")+
    geom_bar(stat="identity", position="dodge")+
    geom_text(mapping=aes(x=industry, y=prop, label=pct),
              vjust = -0.75, size = 3,
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    geom_hline(yintercept=0, size=1.05) +
    scale_fill_manual( values = cPallette) +
    scale_y_continuous(limits=c(0,60), breaks = seq(0,60,by = 10),  label=percent) +
    theme_codemog(base_size=base) +
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Base Indistries",
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x  = element_text(angle=90, vjust=-0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "none")


  # Table data
  f.jobsBaseTab$pos <- if_else(f.jobsBaseTab$industry  == "employment",4,
                               if_else(f.jobsBaseTab$industry == "ib_emp", 2,
                                       if_else(f.jobsBaseTab$industry == "direct_basic_emp",1,3)))

  f.jobsBaseTab$industry <- if_else(f.jobsBaseTab$industry  == "employment","Total Employment",
                                    if_else(f.jobsBaseTab$industry == "ib_emp", "Indirect Basic Employment",
                                            if_else(f.jobsBaseTab$industry == "direct_basic_emp","Direct Basic Employment","Local Services Employment")))

  totalEmp <- as.numeric(f.jobsBaseTab[1,6])

  f.jobsBaseTab$pct <- percent((f.jobsBaseTab$jobs/totalEmp)*100)
  f.jobsBaseTab$jobs <- format(round(f.jobsBaseTab$jobs,digits=0),big.mark=",")

  m.jobs <-   as.matrix(f.jobsBaseTab[order(f.jobsBaseTab$pos),c(5,6,8)])
  names_spaced <- c("Employment Type","Number of Jobs","Percentage")

  #Table Output
  if(oType == "html") {
    jobsTab <- m.jobs %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrr',
            caption=paste0("Jobs by Sector: ",ctyname, ", ",curyr),
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "2in") %>%
      column_spec(2, width = "0.75in") %>%
      column_spec(3, width = "0.75in") %>%
      add_footnote(captionSrc("SDO",""))
  }

  if(oType == "latex") {
    jobsTab <- m.jobs%>% kable(digits=1,
                               row.names=FALSE,
                               align='lrrr',
                               caption=paste0("Jobs by Sector: ",ctyname, ", ",curyr),
                               format="latex", booktabs=TRUE,
                               col.names = names_spaced)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "2in") %>%
      column_spec(2, width = "0.75in") %>%
      column_spec(3, width = "0.75in") %>%
      add_footnote(captionSrc("SDO",""))
  }
  #preparing data

  f.jobsBaseFin <- f.jobsBaseFin[,c(4:6,8)]
  f.jobsBaseFin$jobs <- round(f.jobsBaseFin$jobs,digits=0)

    names(f.jobsBaseFin) <- c("Place","Industry","Jobs","Percentage")

  outList <- list("plot" = BaseBars,"table" = jobsTab, "data1"=f.jobsBaseFin, "data2" = f.jobsBaseTab)
  return(outList)
}
