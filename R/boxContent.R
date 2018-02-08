#' boxContent outputs the HTML code for content and buttons of the various info boxes
#'
#'    need  to put the box text in a HTML() call to render
#'
#' @param title The title of the box content
#' @param description The long description of the Box
#' @param stats A T/F value to output information about statistical tests
#' @param MSA Test describing the MSA adjustment for counties in the Denver MSA
#' @param table A T/F value to outout information about the output table
#' @param urlList A list of output links to sources c(name, URL)
#'
#' @return  Content and buttons for a specified info box,
#'
#' @export
#'
boxContent <- function(title,description, source, MSA, stats, table, urlList) {
  outList <- list()
  i <- 1
  ui0 <- ""
  ui1 <- ""
  ui2 <- ""
  ui3 <- ""
  ui4 <- ""
  ui5 <- ""
  ui6 <- ""
  ui7 <- ""

  ui0 <- tags$b(title)
  outList[[i]] <- ui0
  i <- i + 1


  outList[[i]] <- tags$div(tags$p(description))
  i <- i + 1

  # MSA Block
  if(MSA == "T") {
    ui2 = tags$p("Statistics for the counties in the Denver Metropolitan Statistical Area (Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas and Jefferson) are combined in this section.")
    outList[[i]] <- ui2
    i <- i + 1

  }
  #stats block
  if(stats == "T") {
    ui3 <- tags$p("Estimates of statistically significant differences are calculated at the 90% confidence level.")
    outList[[i]] <- ui3
    i <- i + 1
    ui4 <-  tags$p("For more information on the Margin of Error and its use in statistical testing, see:")
    outList[[i]] <- ui4
    i <- i + 1

    ui5 <- tags$ul(
      tags$li(tags$a(href="https://demography.dola.colorado.gov/demography/understanding-margins-error/","Understanding Margins of Error",target="_blank")),
      tags$li(tags$a(href="https://www.census.gov/programs-surveys/acs/guidance.html","U.S. Census Bureau American Community Survey Guidance for Data Users",target="_blank"))
    )
    outList[[i]] <- ui5
    i <- i + 1

  }

  #Table block
  if(table == "T") {
    ui6 <- tags$p("To download the redered table, click on the 'Submit PDF' button and copy the table from the output PDF report.")
    outList[[i]] <- ui6
    i <- i + 1
  }

  # URLlinks

  urlMatrix <- matrix(unlist(urlList), nrow=length(urlList),ncol=2,byrow = TRUE)
  links <- list()
  for (j in 1:length(urlList)){
    eleHtml <- paste0("<li><a href='",urlMatrix[j,2],"' target='_blank'>",urlMatrix[j,1],"</a></li>")
    links[[j]] <- eleHtml
  }

  ui7 <- tags$ul(HTML(unlist(links)))
  outList[[i]] <- tags$p("Source Information:")
  i <- i + 1
  outList[[i]] <-ui7

  box <- tags$div(outList)

  return(box)
}

