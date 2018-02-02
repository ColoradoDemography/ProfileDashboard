rm(list = ls())
library(tidyverse, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemog)
library(codemogProfile, quietly=TRUE)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL, quietly=TRUE)
library(VennDiagram)
library(shiny)
library(gridExtra)

#' percent: utility function  that ourputs numeric value (typically * 100) as a percentage
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'   @param x is the input numeric value.  The percentage value (y/total *100) is claculated outside of the function
#'   @param digits the number of digits to output
#'   @param format input format
#'
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}


#' simpleCap convers strings to proper case, stolen from Stackoverflow.
#'   @param x input string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

#' roundUpNice: rounds numbers to celing or floor based on inputs  used in setting the range for the COC chart
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'   @param x is the input numeric value.
#'   @param Unit the base value to round around
#'   @return numeric value rounded up to the specific number of digits
#'   @export
roundUpNice <- function(x, Unit) {
  if(x < 0){
    z <- Unit*floor(x/Unit)
  } else {
    z <- Unit*ceiling(x/Unit)
  }

  return(z)
}


#' captionSrc formats the captions for the tables and plots
#' @param type the data type, "SDO" for State Demography Office data, "ACS" for American Community Survey data
#' @param dataSrc The data string for ACS data
#' @return  Date-stamped caption string
#' @export
#'
captionSrc <- function(type, dataSrc, srcPlot = "T") {

  dateStr <- as.character(format(Sys.Date(),"%m/%d/%Y"))

  if(type == "SDO") {
    if(srcPlot == "T") {
    srcStr <- paste0("Source: State Demography Office ", dateStr)
      } else {
      srcStr <- "State Demography Office"
      }
  }

  if(type == "SDOBEA") {
    srcStr <- paste0("Source: State Demography Office and U.S. Bureau of Economic Analysis ", dateStr)
  }

  if(type == "BEA") {
    srcStr <- "U.S. Bureau of Economic Analysis"
  }

  if(type == "LODES") {
    if(srcPlot == "T") {
    srcStr <- paste0("Source: U.S. Census Bureau LEHD Origin-Destination Employment Statistics (LODES) ", dateStr)
    } else {
      srcStr <- "U.S. Census Bureau LEHD Origin-Destination Employment Statistics (LODES)"
    }
  }
  if(type == "QCEW") {
    if(srcPlot == "T") {
      srcStr <- paste0("Source: Department of Labor and Employment (QCEW) ", dateStr)
    } else{
      srcStr <- "Department of Labor and Employment (QCEW)"
    }
  }
  if(type =="ACS") {
    byr <- paste0("20",substr(dataSrc,4,5))
    eyr <- paste0("20",substr(dataSrc,6,7))
    if(srcPlot == "T") {
    srcStr <- paste0("Source: U.S. Census Bureau, ",byr,"-",eyr," American Community Survey. ", dateStr)
    } else {
      srcStr <- paste0("U.S. Census Bureau, ",byr,"-",eyr," American Community Survey.")
    }
  }
  return(srcStr)
}


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
                       gp=gpar(fontsize=9),
                       x = unit(1, "npc"),
                       hjust = 1,
                       vjust = 0)

  outVenn <- arrangeGrob(g, lg, nrow=3, ncol=1, heights=c(4,1,1),
                         top=textGrob(location, gp=gpar(fontsize=15,font=8)), sub=sub.label)
  return(outVenn)
}

fips = "001"
ctyname = "Adams County"
ACS = "acs1216"

x <- GenerateVenn(fips=fips, ctyname=ctyname,oType= "html")

grid.draw(x)

grid.arrange(x)
x
ggsave("test.png",x)
 y <- renderPlot(x)
 plotOutput(y)
