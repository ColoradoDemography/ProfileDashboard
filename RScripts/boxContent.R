#' boxContent outputs the HTML code for content and buttons of the various info boxes
#'
#'    need  to put the box text in a HTML() call to render
#'
#' @param title The title of the box content
#' @param description The long description of the Box
#' @param source data source citation
#' @param MSA   A T/F flag describing the MSA adjustment for counties in the Denver MSA
#' @param stats A T/F value to output information about statistical tests
#' @param muni  A T/F flag indicating whether information is available for municipalities
#' @param PlFilter a TRUE/FALSE flag identifying municipalities smaller than 200 people
#' @param table A T/F value to outout information about the output table
#' @param urlList A list of output links to sources c(name, URL)
#'
#' @return  Content and buttons for a specified info box,
#'
#' @export
#'
boxContent <- function(title,description, source, MSA, stats, muni, multiCty, PlFilter, table, urlList) {
  outList <- list()
  i <- 1
  ui00 <- ""
  ui01 <- ""
  ui02 <- ""
  ui03 <- ""
  ui04 <- ""
  ui05 <- ""
  ui06 <- ""
  ui07 <- ""
  ui08 <- ""
  ui09 <- ""
  ui10 <- ""
  
  ui00 <- tags$b(title)
  outList[[i]] <- ui00
  i <- i + 1


  outList[[i]] <- tags$div(tags$p(description))
  i <- i + 1

  # MSA Block
  if(MSA == "T") {
    ui02 = tags$p("Statistics for the counties in the Denver Metropolitan Statistical Area (Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas and Jefferson) are combined in this section.")
    outList[[i]] <- ui02
    i <- i + 1

  }
  #stats block
  if(stats == "T") {
    ui03 <- tags$p("Estimates of statistically significant differences are calculated at the 90% confidence level.")
    outList[[i]] <- ui03
    i <- i + 1
    ui04 <-  tags$p("For more information on the Margin of Error and its use in statistical testing, see:")
    outList[[i]] <- ui04
    i <- i + 1

    ui05 <- tags$ul(
      tags$li(tags$a(href="https://demography.dola.colorado.gov/demography/understanding-margins-error/","Understanding Margins of Error",target="_blank")),
      tags$li(tags$a(href="https://www.census.gov/programs-surveys/acs/guidance.html","U.S. Census Bureau American Community Survey Guidance for Data Users",target="_blank"))
    )
    outList[[i]] <- ui05
    i <- i + 1

  }
  
  # muni block
  if(muni == "T") {
    ui06 <- tags$p("Projections and estimates are not availaible for municipalities.  Please contact the SDO office for additional information.")
    outList[[i]] <- ui06
    i <- i + 1
  }
  
  #multiCty
  if(multiCty == "T") {
    ui07 <- tags$p("For municipalities in multiple counties, data for the majoity county are reprted.  Please contact the SDO office for additional information.")
    outList[[i]] <- ui07
    i <- i + 1
  }

  # PlFilter block
  if(PlFilter == "T") {
    ui08 <- tags$p("Municipal estimates are not avaialble for places with fewer than 200 residents.  Please contact the SDO office for additional information.")
    outList[[i]] <- ui08
    i <- i + 1
  }
  
  #Table block
  if(table == "T") {
    ui09 <- tags$p("To download the redered table, click on the 'Submit PDF' button and copy the table from the output PDF report.")
    outList[[i]] <- ui09
    i <- i + 1
  }

  # URLlinks

  urlMatrix <- matrix(unlist(urlList), nrow=length(urlList),ncol=2,byrow = TRUE)
  links <- list()
  for (j in 1:length(urlList)){
    eleHtml <- paste0("<li><a href='",urlMatrix[j,2],"' target='_blank'>",urlMatrix[j,1],"</a></li>")
    links[[j]] <- eleHtml
  }

  ui10 <- tags$ul(HTML(unlist(links)))
  outList[[i]] <- tags$p("Source Information:")
  i <- i + 1
  outList[[i]] <-ui10

  box <- tags$div(outList)

  return(box)
}

