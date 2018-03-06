#' setYrRange Calculates a range of year values, typically in 5-year increments with adjustments
#'   accounting for StartYr and EndYr values that are not multiples of 5
#'
#' @param StartYr a numberic variable for the start of the series
#' @param EndYr a numeric variable for the end of the series
#' @return a numeric vector of the years between StartYr and EndYr accounting for odd years
#' @export
setYrRange <- function(StartYr,EndYr) {

  sRem <- StartYr %% 5
  eRem <- EndYr %% 5

  if((sRem == 0) & (eRem == 0)) { # Regular 5-year sequence
    outSeq <- seq(StartYr,EndYr,5)
  }
  if((sRem != 0) & (eRem == 0)) { # Regular 5-year sequence
    outSeq <- c(StartYr,seq((StartYr+(5-sRem)),EndYr,5))
  }
  if((sRem == 0) & (eRem != 0)) { # Regular 5-year sequence
    outSeq <- c(seq(StartYr,(EndYr-eRem),5),EndYr)
  }
  if((sRem != 0) & (eRem != 0)) { # Regular 5-year sequence
    outSeq <- c(StartYr,seq((StartYr+(5-sRem)),(EndYr-eRem),5),EndYr)
  }
  return(outSeq)
}  #setYrRange
