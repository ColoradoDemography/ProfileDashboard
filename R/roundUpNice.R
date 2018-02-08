#' roundUpNice: rounds numbers to celing or floor based on inputs  used in setting the range for the COC chart
#'
#'   Maintains leading and trailing zeros...
#'   Taken from StackOverFlow 20171122...
#'
#' @param x is the input numeric value.
#' @param Unit the base value to round around
#' @return numeric value rounded up to the specific number of digits
#' @export
roundUpNice <- function(x, Unit) {
  if(x < 0){
    z <- Unit*floor(x/Unit)
  } else {
    z <- Unit*ceiling(x/Unit)
  }

  return(z)
}

