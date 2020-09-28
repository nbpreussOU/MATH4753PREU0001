#' Determine which of two models has the better fit
#'
#'  More detailed description
#'
#' @param m1 model 1
#' @param m2 model 2
#' @return returns a vector with value[1] = TSS, value[2] = MSS, value[3] = RSS
#'
#'
#' @export
#'
fitCompare <- function(m1, m2)
{
  a = summary(m1)$r.squared
  b = summary(m2)$r.squared

  if(a - b < 0 )
  {
    result <- "model 2"
  }else if(a - b > 0)
  {
    result <- "model 1"
  }else
  {
    result <- "same"
  }
  return (list(x, result))
}
