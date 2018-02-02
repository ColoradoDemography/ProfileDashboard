#' Calculates relative width and height for comparison from data.
#' From RobR
#'
#' @param comp Value of variable from comparison group.
#' @param ref Value of reference group, generally larger.
#' @param h Height of the reference square.
#' @param w Width of the reference square.
#' @keywords growth rate, annualized
#' @export


relsize<- function(comp,ref,h,w){
  r=comp/ref
  h1=h*r
  w1=w*r
  x=c(h1,w1,r)
  return(x)
}


