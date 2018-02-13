#' GenerateVenn Generates a Venn diagram using LODES data
#'
#' @param fips is the numeric fips code for county
#' @param ctyname is the cplace name from input$unit
#' @param oType output type html or latex
#' @return ggplot2 graphic, formatted datatables, and datasets
#' @export
#'
GenerateVenn <- function(fips, ctyname,oType){
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

  # Read data files f.xwalk and f.alljobs
  f.xwalk <- dbGetQuery(con, "SELECT * FROM data.coxwalk;")
  f.alljobs <- dbGetQuery(con, "SELECT * FROM data.colodesblk15;")

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)


  selPlace <- paste0("08",fips)
  if(nchar(fips) == 3) {  #Selecting blocks based on cty value
    f.selXWalk <- f.xwalk[which(f.xwalk$cty == selPlace), c(1,5,6)]
  }
  if(nchar(fips) == 4) { #Selecting blocks based on zctaname  value
    selPlace <- paste0("8",fips)
    f.selXWalk <- f.xwalk[which(f.xwalk$zctaname == selPlace), c(1,14,18)]
  }
  if (nchar(fips) == 5) { #Selecting blocks based on stplc value
    f.selXWalk <- f.xwalk[which(f.xwalk$stplc == selPlace), c(1,15,16)]
  }

  #Creating Master Block List
  selList <- f.selXWalk[[1]]

  #Outputting block data for All Jobs

  #Creating merge
  f.allBlocks_fin <- f.alljobs[which((f.alljobs$h_geocode %in% selList) | (f.alljobs$w_geocode %in% selList)), ]


  #Counting up the number of jobs
  f.allBlocks_fin$live_out_work_in <-ifelse((!(f.allBlocks_fin$h_geocode %in% selList) & (f.allBlocks_fin$w_geocode %in% selList)),f.allBlocks_fin$s000,0)
  f.allBlocks_fin$live_in_work_out <-ifelse(((f.allBlocks_fin$h_geocode %in% selList) & !(f.allBlocks_fin$w_geocode %in% selList)),f.allBlocks_fin$s000,0)
  f.allBlocks_fin$live_in_work_in <-ifelse(((f.allBlocks_fin$h_geocode %in% selList) & (f.allBlocks_fin$w_geocode %in% selList)),f.allBlocks_fin$s000,0)

  # Potentially identify the jobs out of state by taking the live_in_work_out jobs and classifying them by the state of
  # w_geocode  i.e. if substr(w_geocode,1,2) == "08" is a someone working in state, otherwise, workign out of state.

  # Summarizing the counties for live in area, work elsewhere (Work_out) and work in area but live elsewhere (live_out)
  f.work_out <- f.allBlocks_fin[which(f.allBlocks_fin$live_in_work_out > 0),]
  f.live_out <- f.allBlocks_fin[which(f.allBlocks_fin$live_out_work_in > 0),]

  if(nchar(fips) == 3) { #cty
    f.work_o1 <- f.work_out %>%
      group_by(w_geocode) %>%
      summarise(lin_wout = sum(live_in_work_out))

    f.work_o2 <- merge(f.work_o1, f.xwalk, by.x="w_geocode",by.y="tabblk2010")
    f.work_fin <- f.work_o2 %>%
      group_by(ctyname) %>%
      summarise(lin_wout_fin = sum(lin_wout)) %>%
      arrange(desc(lin_wout_fin))

    f.live_o1 <- f.live_out %>%
      group_by(h_geocode) %>%
      summarise(lout_win = sum(live_out_work_in),
                lin_win = sum(live_in_work_in))

    f.live_o2 <- merge(f.live_o1, f.xwalk, by.x="h_geocode",by.y="tabblk2010")
    f.live_fin <- f.live_o2 %>%
      group_by(ctyname) %>%
      summarise(lout_win_fin = sum(lout_win)) %>%
      arrange(desc(lout_win_fin))
  }


  #Generating Venn Diagrams

  #Collapsing datasets for venn diagrams
  if(nchar(fips) == 3) { #cty
    f.allBlocks_sum <- f.allBlocks_fin %>%
      summarise( lout_win = sum(live_out_work_in),
                 lin_wout = sum(live_in_work_out),
                 lin_win = sum(live_in_work_in))
  }

  if(nchar(fips) == 4) { #zctaname
    f.allBlocks_sum <- f.allBlocks_fin %>%
      summarise( lout_win = sum(live_out_work_in),
                 lin_wout = sum(live_in_work_out),
                 lin_win = sum(live_in_work_in))
  }

  if(nchar(fips) == 5) { #stplc
    f.allBlocks_sum <- f.allBlocks_fin %>%
      summarise( lout_win = sum(live_out_work_in),
                 lin_wout = sum(live_in_work_out),
                 lin_win = sum(live_in_work_in))
  }


  location <- paste0(ctyname,"\n","All Jobs")

  lout_win <- as.numeric(f.allBlocks_sum$lout_win)
  lin_wout <- as.numeric(f.allBlocks_sum$lin_wout)
  lin_win <-  as.numeric(f.allBlocks_sum$lin_win)

  region1 <- lout_win + lin_win #Live outside, work in
  region2 <- lin_wout + lin_win #Live in, woek outside
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
  } #End I Loop


  #Building Legend
  cols <- c("chartreuse4", "aquamarine2","aquamarine3")
  lg <- legendGrob(labels=c("Employed in Selected Area, Live Outside ",
                            "Live in Selected Area, Employed Outside",
                            "Employed and Live in Selected Area"),
                   pch=rep(19,length(c("Employed in Selected Area, Live Outside ",
                                       "Live in Selected Area, Employed Outside",
                                       "Employed and Live in Selected Area"))),

                   gp=gpar(col=cols, fill="gray", fontsize=10),
                   byrow=TRUE)

  g <- gTree(children = gList(diag))


  #outVenn is the final VennDiagram
  #Formatting citation
  sub.label = textGrob(captionSrc("LODES",""),
                       gp=gpar(fontsize=8),
                       x = unit(1, "npc"),
                       hjust = 1,
                       vjust = 0)

    outVenn <- arrangeGrob(g, lg, nrow=3, ncol=1, heights=c(4,1,1),
                         top=textGrob(location, gp=gpar(fontsize=15,font=8)), sub=sub.label)

  options(warn=0)  # restoring Warning Messages
  # Finalizing the output data sets
  #selecting the top 10 places

  f.work_fin$pctWorkOut <- (f.work_fin$lin_wout_fin/lin_wout)*100

  f.work_fin10 <- f.work_fin[1:10,]

  woutRem <-  lin_wout - as.numeric(sum(f.work_fin10$lin_wout_fin))
  woutSum10 <- 100 - as.numeric(sum(f.work_fin10$pctWorkOut))
  woutL <- data.frame(ctyname = "Other Counties",
                      lin_wout_fin = woutRem,
                      pctWorkOut = woutSum10)

  f.work_fin11 <- rbind(f.work_fin10,woutL)
  f.work_fin11$lin_wout_fin <- format(f.work_fin11$lin_wout_fin,big.mark=",")
  f.work_fin11$pctWorkOut <- percent(f.work_fin11$pctWorkOut)
  names(f.work_fin) <- c("Location","Work Location","Percentage")

  f.live_fin$pctLiveOut <- (f.live_fin$lout_win_fin/lout_win)*100
  f.live_fin10 <- f.live_fin[1:10,]

  loutRem <-  lout_win - as.numeric(sum(f.live_fin10$lout_win_fin))
  loutSum10 <- 100 - as.numeric(sum(f.live_fin10$pctLiveOut))
  loutL <- data.frame(ctyname = "Other Counties",
                      lout_win_fin = loutRem,
                      pctLiveOut = loutSum10)
  f.live_fin11 <- rbind(f.live_fin10,loutL)
  f.live_fin11$lout_win_fin <- format(f.live_fin11$lout_win_fin,big.mark=",")
  f.live_fin11$pctLiveOut <- percent(f.live_fin11$pctLiveOut)

  names(f.live_fin) <- c("Location","Residence Location","Percentage")



  # Formatting Work Output table.
  names_spaced <- c("Location","Count","Percent")
  capstr1 <- paste0("Employees in ",ctyname," but living elsewhere")
  capstr2 <- paste0("Residents of ",ctyname," but working elsewhere")

  m.work11 <- as.matrix(f.work_fin11)
  m.live11 <- as.matrix(f.live_fin11)

  if(oType == "html") {

  work_tab <- m.work11 %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr1,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    add_footnote(captionSrc("LODES",""))


  #formatting Live output table
  live_tab <- m.live11 %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr2,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 11) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    add_footnote(captionSrc("LODES",""))



  # Binding List for Output
  outList <- list("plot" = outVenn, "tab1" = work_tab, "data1" = f.work_fin, "dataTab1" = f.work_fin11,
                  "tab2" = live_tab, "data2" = f.live_fin, "dataTab2" = f.live_fin11)
    }

  if(oType == "latex") {

  workTab <-kable(m.work11,
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


  liveTab <-kable(m.live11,
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

