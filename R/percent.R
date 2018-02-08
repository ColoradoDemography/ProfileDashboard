#' percent: utility function  that ourputs numeric value (typically * 100) as a percentage
#'
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#' @param x is the input numeric value.  The percentage value (y/total *100) is claculated outside of the function
#' @param digits the number of digits to output
#' @param format input format
#' @return a numeric string formatted as a percentage
#' @export
#'
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}
