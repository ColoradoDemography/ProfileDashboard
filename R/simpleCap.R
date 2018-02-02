#' simpleCap convers strings to proper case,
#'
#' stolen from Stackoverflow.
#'
#' @param x input string
#' @return String formatted in Proper Case
#' @export
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}
