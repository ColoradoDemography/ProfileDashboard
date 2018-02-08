#' ms_jobs Creates a Chart showing the Total Estimates Jobs series for each County in Colorado
#'
#' Modified from ms_jobs January, 2018 AB
#'
#' Uses State Demography Office data to create a chart showing the timeseries of Total Estimated Jobs
#' (which means it includes Proprietors and Agricultural Workers) for a selected Colorado County
#'
#' @param fips is the fips code for the county being examined
#' @param ctyname  This parameter puts the name of the county in the chart
#' @param maxyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot graphic and data file
#' @export
#'
ms_jobs=function(fips, ctyname, maxyr,base=10){
  jobs_data <- county_jobs(as.numeric(fips), 2001:maxyr) %>%
    mutate(jobs=car::recode(totalJobs, "'S'=NA"),
           jobs=round(as.numeric(jobs),0),
           year=as.numeric(as.character(year)))

  jobs_plot <- jobs_data %>%
    ggplot(aes(x=year, y=as.numeric(jobs)))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2002, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_line(color=rgb(0, 168, 58, max = 255), size=1.5)+
    scale_x_continuous(breaks=c(2001:maxyr))+
    scale_y_continuous(labels=comma)+
    theme_codemog(base_size=base)+
    labs(x="Year",
         y="Jobs",
         title= paste0("Total Estimated Jobs, 2001 to ",as.character(maxyr)),
         subtitle = ctyname,
         caption= paste0(captionSrc("SDO","") ,"\nNote: Grey shading represents beginning to bottom of U.S. recessions")) +
    theme(plot.title = element_text(hjust = 0.5, size=18))

  outList <- list("plot" = jobs_plot, "data" = jobs_data)
  return(outList)
}
