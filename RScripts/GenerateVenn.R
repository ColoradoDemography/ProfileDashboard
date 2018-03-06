#' GenerateVenn Generates a Venn diagram using LODES data
#' V2 revised 2/15/2018 AB
#' @param fips is the numeric fips code for counties and municiaplities/places
#' @param level the measurement uit, taken from input$level
#' @param ctyname is the place name from input$unit
#' @param oType output type html or latex
#' @return ggplot2 graphic, formatted datatables, and datasets
#' @export
#'
GenerateVenn <- function(fips, level, ctyname,oType){

  options(warn=-1)  # Suppressing warning messages produced by VennDiagram

 if(nchar(fips) == 3) {
   sumSQL <- paste0("SELECT * FROM data.otm_county_summary WHERE fips = '",fips,"' ;")
   placeSQL <- paste0("SELECT * FROM data.otm_county_place WHERE fips = '",fips,"' ;")
 }
if(nchar(fips) == 5) {
  sumSQL <- paste0("SELECT * FROM data.otm_place_summary WHERE fips = '",fips,"' ;")
  placeSQL <- paste0("SELECT * FROM data.otm_place_place WHERE fips = '",fips,"' ;")
}
 
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

  # Read data files f.xwalk and f.alljobs

    f.summary <- dbGetQuery(con, sumSQL)
    f.place <- dbGetQuery(con, placeSQL)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  location <- paste0(ctyname,"\n","All Jobs, ",as.character(f.summary$year))

  lout_win <- as.numeric(f.summary$workin_liveout)
  lin_wout <- as.numeric(f.summary$livein_workout)
  lin_win <-  as.numeric(f.summary$livein_workin)

  region1 <- lout_win + lin_win #Live outside, work in
  region2 <- lin_wout + lin_win #Live in, work outside
  
  crossRegion <- lin_win 

 
  # By default, VennDiagram outputs the larger Region value in the left hand postion.
  # This code block insures that the diagram is correct
  if(lin_wout >= lout_win){
    diag <- draw.pairwise.venn(region1, region2, crossRegion, inverted = TRUE,
                               lty = rep("solid", 2), cat.col = rep("black", 2),
                               cex = 1, cat.cex = 1,
                               fill = c("chartreuse4", "aquamarine2"), alpha = rep(0.5, 2),
                               euler.d=TRUE,scaled=TRUE, ind = FALSE, print.mode="raw")
  } else{
    diag <- draw.pairwise.venn(region1, region2, crossRegion, inverted = FALSE,
                               lty = rep("solid", 2), cat.col = rep("black", 2),
                               cex = 1, cat.cex = 1,
                               fill = c("chartreuse4", "aquamarine2"), alpha = rep(0.5, 2),
                               euler.d=TRUE,scaled=TRUE, ind = FALSE, print.mode="raw")
  }


  # Formatting the labels for the output diagram
  # Change labels for first three text grobs
  # hard-coded three, but it would be the number of text labels
  # minus the number of groups passed to venn.diagram
  
 
  idx <- sapply(diag, function(i) grepl("text", i$name))

  for(i in 1:3){
    diag[idx][[i]]$label <-
      format(as.numeric(diag[idx][[i]]$label), big.mark=",", scientific=FALSE)
    if(diag[idx][[i]]$label == "NA") {
      diag[idx][[i]]$label <- ""
    }
  } #End I Loop


  #Building Legend
  cols <- c("chartreuse4", "aquamarine2","aquamarine3")
  lg <- legendGrob(labels=c("Employed in Selected Area, Live Outside ",
                            "Live in Selected Area, Employed Outside",
                            "Employed and Live in Selected Area"),
                   pch=rep(19,length(c("Employed in Selected Area, Live Outside ",
                                       "Live in Selected Area, Employed Outside",
                                       "Employed and Live in Selected Area"))),

                   gp=gpar(col=cols, fill="gray", fontsize=12),
                   byrow=TRUE)

  g <- gTree(children = gList(diag))


  #outVenn is the final VennDiagram
  #Formatting citation
  sub.label = textGrob(captionSrc("LODES",""),
                       gp=gpar(fontsize=12),
                       x = unit(1, "npc"),
                       hjust = 1,
                       vjust = 0)

    outVenn <- arrangeGrob(g, lg, nrow=3, ncol=1, heights=c(4,1,1),
                         top=textGrob(location, gp=gpar(fontsize=15,font=8)), sub=sub.label)

  options(warn=0)  # restoring Warning Messages
  # Finalizing the output data sets
  #selecting the top 10 places

  f.work_fin <- f.place[which(f.place$type == 1),c(5:7)]
  names(f.work_fin) <- c("Location","Count","Percent")
  f.work_sum <- f.work_fin %>%
      summarize(Count = sum(Count),
                Percent = sum(Percent))
  f.work_sum$Location <- "Total"
  f.work_sum <- f.work_sum[,c(3,1,2)]
  f.work_fin <- rbind(f.work_fin,f.work_sum)
  
  f.work_fin$Count <- format(f.work_fin$Count,big.mark=",")
  f.work_fin$Percent <- percent(f.work_fin$Percent)
  


  f.live_fin <- f.place[which(f.place$type == 2),c(5:7)]
  names(f.live_fin) <- c("Location","Count","Percent")
  f.live_sum <- f.live_fin %>%
    summarize(Count = sum(Count),
              Percent = sum(Percent))
  f.live_sum$Location <- "Total"
  f.live_sum <- f.live_sum[,c(3,1,2)]
  f.live_fin <- rbind(f.live_fin,f.live_sum)
  
  f.live_fin$Count <- format(f.live_fin$Count,big.mark=",")
  f.live_fin$Percent <- percent(f.live_fin$Percent)




  # Formatting Work Output table.
  names_spaced <- c("Location","Count","Percent")
  capstr1 <- paste0("Employees in ",ctyname," but living elsewhere")
  capstr2 <- paste0("Residents of ",ctyname," but working elsewhere")

  m.work <- as.matrix(f.work_fin)
  m.live <- as.matrix(f.live_fin)

  if(oType == "html") {

  work_tab <- m.work %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr1,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = T) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    add_footnote(captionSrc("LODES",""))


  #formatting Live output table
  live_tab <- m.live %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr2,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = T) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    add_footnote(captionSrc("LODES",""))



  # Binding List for Output
  outList <- list("plot" = outVenn, "tab1" = work_tab, "data1" = f.work_fin,
                  "tab2" = live_tab, "data2" = f.live_fin)
    }

  if(oType == "latex") {

  workTab <-kable(m.work,
                  col.names = names_spaced,
                 row.names=FALSE,
                 align='lrr',
                 caption=capstr1,
                 format="latex", booktabs=TRUE) %>%
    kable_styling(latex_options="HOLD_position") %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "2in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.5in") %>%
    add_footnote(captionSrc("LODES",""))


  liveTab <-kable(m.live,
                  col.names = names_spaced,
                  row.names=FALSE,
                  align='lrr',
                  caption=capstr2,
                  format="latex", booktabs=TRUE) %>%
    kable_styling(latex_options="HOLD_position") %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "2in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.5in") %>%
    add_footnote(captionSrc("LODES",""))

  outList <- list("plot" = outVenn, "workTab" = workTab,"liveTab" = liveTab)
  }

  return(outList)
  }

