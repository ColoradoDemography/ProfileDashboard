#' jobsByIndustry Creates a Chart showing the Total estimated jobs by sector for
#'   a place and for Colorado
#'
#' Uses State Demography Office data to create a chart compaing jobs by major sector
#' for a selected place, compared to Colorado.
#'
#' @param fips is the fips code for the county being examined
#' @param ctyname  This parameter puts the name of the county in the chart
#' @param curyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme(), defaults to base = 10
#' @return  ggplot graphic and data file
#' @export
#'
jobsByIndustry <- function(fips, ctyname, curyr, base=10){
  options(warn=-1)  # Suppressing warning messages produced by VennDiagram
  #Reading data
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
  f.jobsPL <- dbGetQuery(con, paste0("SELECT * FROM estimates.jobs_by_sector WHERE (area_code = ", as.character(as.numeric(fips)),
                                     " AND population_year = ", as.character(curyr), ");"))
  f.jobsST <- dbGetQuery(con, paste0("SELECT * FROM estimates.jobs_by_sector WHERE (area_code = 0 AND population_year = ",
                                     as.character(curyr), ");"))

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  f.jobsPL$total_jobs <- if_else(is.na(f.jobsPL$total_jobs),0,f.jobsPL$total_jobs)
  f.jobsPL$sector_name <- gsub("&nbsp;","",f.jobsPL$sector_name)
  f.jobsPL$sector_id <- if_else(nchar(f.jobsPL$sector_id) == 4,paste0("0",f.jobsPL$sector_id), f.jobsPL$sector_id)

  # Assigning and extracginf major Categories
  f.jobsPL$sector_agg <- if_else(f.jobsPL$sector_id == "01000",1,
                                 if_else(f.jobsPL$sector_id == "02000",1,
                                         if_else(f.jobsPL$sector_id == "03000",1,
                                                 if_else(f.jobsPL$sector_id == "04000",1,
                                                         if_else(f.jobsPL$sector_id == "05000",1,
                                                                 if_else(f.jobsPL$sector_id == "06000",1,
                                                                         if_else(f.jobsPL$sector_id == "07000",1,
                                                                                 if_else(f.jobsPL$sector_id == "08000",1,
                                                                                         if_else(f.jobsPL$sector_id == "09000",1,
                                                                                                 if_else(f.jobsPL$sector_id == "10000",1,
                                                                                                         if_else(f.jobsPL$sector_id == "10150",1,
                                                                                                                 if_else(f.jobsPL$sector_id == "11000",1,
                                                                                                                         if_else(f.jobsPL$sector_id == "11025",1,
                                                                                                                                 if_else(f.jobsPL$sector_id == "11050",1,
                                                                                                                                         if_else(f.jobsPL$sector_id == "12000",1,
                                                                                                                                                 if_else(f.jobsPL$sector_id == "12015",1,
                                                                                                                                                         if_else(f.jobsPL$sector_id == "13000",1,
                                                                                                                                                                 if_else(f.jobsPL$sector_id == "13015",1,
                                                                                                                                                                         if_else(f.jobsPL$sector_id == "14000",1,
                                                                                                                                                                                 if_else(f.jobsPL$sector_id == "15000",1,0))))))))))))))))))))



  f.jobsPLMain <- f.jobsPL[which(f.jobsPL$sector_agg == 1),]

  # Updating category names
  f.jobsPLMain$sector_name <- if_else(f.jobsPLMain$ sector_name == "Accommodation and food","Accomodation and Food Services",
                                      if_else(f.jobsPLMain$ sector_name == "Admin and waste","Adminstration and Waste Services",
                                              if_else(f.jobsPLMain$ sector_name == "Arts","Arts, Entertainment and Recreation",
                                                      if_else(f.jobsPLMain$ sector_name == "Education","Educational Services",
                                                              if_else(f.jobsPLMain$ sector_name == "Finance activities","Finance and Insurance",
                                                                      if_else(f.jobsPLMain$ sector_name == "Health Services","Healthcare and Social Assistance",
                                                                              if_else(f.jobsPLMain$ sector_name == "Management of companies and enterprise","Management of Companies",
                                                                                      if_else(f.jobsPLMain$ sector_name == "Other services, except public administration","Other Services",
                                                                                              if_else(f.jobsPLMain$ sector_name == "Professional and business services","Professional and Technical Services",
                                                                                                      if_else(f.jobsPLMain$ sector_name == "Real estate","Real Estate and Rental and Leasing",
                                                                                                              if_else(f.jobsPLMain$ sector_name == "Wholesale trade","Wholesale Trade",
                                                                                                                      if_else(f.jobsPLMain$ sector_name == "Transportation and warehousing","Transportation and Warehousing",f.jobsPLMain$sector_name
                                                                                                                      ))))))))))))


  f.jobsPLMain <- f.jobsPLMain %>%
    mutate(PLTotal = sum(total_jobs),
           total_c = comma(round(total_jobs,digits=0)),
           prop_jobs = (total_jobs/PLTotal)*100,
           pct_jobs = percent(prop_jobs))


  f.jobsPLMain$geoname <- ctyname
  f.jobsPLMainFin <- f.jobsPLMain[,c(4,13,5,10,11,12)]

  #Preparing State Data set
  f.jobsST$sector_name <- gsub("&nbsp;","",f.jobsST$sector_name)
  f.jobsST$sector_id <- if_else(nchar(f.jobsST$sector_id) == 4,paste0("0",f.jobsST$sector_id), f.jobsST$sector_id)

  # Assigning and extracginf major Categories
  f.jobsST$sector_agg <- if_else(f.jobsST$sector_id == "01000",1,
                                 if_else(f.jobsST$sector_id == "02000",1,
                                         if_else(f.jobsST$sector_id == "03000",1,
                                                 if_else(f.jobsST$sector_id == "04000",1,
                                                         if_else(f.jobsST$sector_id == "05000",1,
                                                                 if_else(f.jobsST$sector_id == "06000",1,
                                                                         if_else(f.jobsST$sector_id == "07000",1,
                                                                                 if_else(f.jobsST$sector_id == "08000",1,
                                                                                         if_else(f.jobsST$sector_id == "09000",1,
                                                                                                 if_else(f.jobsST$sector_id == "10000",1,
                                                                                                         if_else(f.jobsST$sector_id == "10150",1,
                                                                                                                 if_else(f.jobsST$sector_id == "11000",1,
                                                                                                                         if_else(f.jobsST$sector_id == "11025",1,
                                                                                                                                 if_else(f.jobsST$sector_id == "11050",1,
                                                                                                                                         if_else(f.jobsST$sector_id == "12000",1,
                                                                                                                                                 if_else(f.jobsST$sector_id == "12015",1,
                                                                                                                                                         if_else(f.jobsST$sector_id == "13000",1,
                                                                                                                                                                 if_else(f.jobsST$sector_id == "13015",1,
                                                                                                                                                                         if_else(f.jobsST$sector_id == "14000",1,
                                                                                                                                                                                 if_else(f.jobsST$sector_id == "15000",1,0))))))))))))))))))))



  f.jobsSTMain <- f.jobsST[which(f.jobsST$sector_agg == 1),]

  # Updating category names
  f.jobsSTMain$sector_name <- if_else(f.jobsSTMain$ sector_name == "Accommodation and food","Accomodation and Food Services",
                                      if_else(f.jobsSTMain$ sector_name == "Admin and waste","Adminstration and Waste Services",
                                              if_else(f.jobsSTMain$ sector_name == "Arts","Arts, Entertainment and Recreation",
                                                      if_else(f.jobsSTMain$ sector_name == "Education","Educational Services",
                                                              if_else(f.jobsSTMain$ sector_name == "Finance activities","Finance and Insurance",
                                                                      if_else(f.jobsSTMain$ sector_name == "Health Services","Healthcare and Social Assistance",
                                                                              if_else(f.jobsSTMain$ sector_name == "Management of companies and enterprise","Management of Companies",
                                                                                      if_else(f.jobsSTMain$ sector_name == "Other services, except public administration","Other Services",
                                                                                              if_else(f.jobsSTMain$ sector_name == "Professional and business services","Professional and Technical Services",
                                                                                                      if_else(f.jobsSTMain$ sector_name == "Real estate","Real Estate and Rental and Leasing",
                                                                                                              if_else(f.jobsSTMain$ sector_name == "Wholesale trade","Wholesale Trade",
                                                                                                                      if_else(f.jobsSTMain$ sector_name == "Transportation and warehousing","Transportation and Warehousing",f.jobsSTMain$sector_name
                                                                                                                      ))))))))))))


  f.jobsSTMain <- f.jobsSTMain %>%
    mutate(STTotal = sum(total_jobs),
           total_c = comma(round(total_jobs,digits=0)),
           prop_jobs = (total_jobs/STTotal)*100,
           pct_jobs = percent(prop_jobs))


  f.jobsSTMain$geoname <- "Colorado"
  f.jobsSTMainFin <- f.jobsSTMain[,c(4,13,5,10,11,12)]

  f.jobsChart <- rbind(f.jobsSTMainFin,f.jobsPLMainFin)
  f.jobsChart$geoname <- factor(f.jobsChart$geoname, levels = c(ctyname,"Colorado"))

  f.jobsChart$sector_name <- factor(f.jobsChart$sector_name ,
                                    levels = c(
                                      "Government",
                                      "Other Services",
                                      "Accomodation and Food Services",
                                      "Arts, Entertainment and Recreation",
                                      "Healthcare and Social Assistance",
                                      "Educational Services",
                                      "Adminstration and Waste Services",
                                      "Management of Companies",
                                      "Professional and Technical Services",
                                      "Real Estate and Rental and Leasing",
                                      "Finance and Insurance",
                                      "Information",
                                      "Transportation and Warehousing",
                                      "Retail Trade",
                                      "Wholesale Trade",
                                      "Manufacturing",
                                      "Construction",
                                      "Utilities",
                                      "Mining",
                                      "Agriculture"
                                    ))

  pltTitle <- paste0(as.character(curyr)," Share of Jobs by Industry")
  subTitle <- ctyname  #The is the county Name...


  p.jobs <- ggplot(f.jobsChart, aes(x=sector_name, y=prop_jobs, fill=geoname)) +
    geom_bar(stat="identity", position="dodge")+
    geom_text(mapping=aes(x=sector_name, y=prop_jobs, label=pct_jobs),
              hjust = -0.5, size = 2,
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    scale_y_continuous(limits= c(0,20), breaks=seq(0,20, by = 5), expand = c(0, 0), label=percent)  +
    scale_fill_manual(values=c("#6EC4E8","#00953A"), name="Geography")+
    theme_codemog(base_size=base) + coord_flip() +
    theme(axis.text.x=element_text(angle=0))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Job Sector",
         y = "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")



  # Building Output data

  f.jobsFin <- merge(f.jobsPLMainFin,f.jobsSTMainFin, by= "sector_id")
  f.jobsFin <- f.jobsFin[order(f.jobsFin$sector_id),]
  f.jobsFin <- f.jobsFin[,c(3,4,6,9,11)]
  names(f.jobsFin) <- c("Job Sector", paste0("Number of Jobs: ",ctyname), paste0("Percentage of Jobs: ",ctyname),
                        "Number of Jobs: Colorado","Percentage of Jobs: Colorado")

  outList <- list("plot" = p.jobs,"data"= f.jobsFin)
  return(outList)

}
