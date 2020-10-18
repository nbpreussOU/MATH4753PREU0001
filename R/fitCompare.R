#' Determine which of two models has the better fit
#'
#'  More detailed description
#'
#' @param m1 model 1
#' @param m2 model 2
#' @return returns a list with text relating which model had a better adjusted r-squared
#'
#' @examples
#' x <- c(1:20)
#' y <- c(2:21)
#' z <- c(1,4,6,8,1,2,6,4,1,2,3,4,6,2,3,8,6,4,3,2)
#' fitCompare(stats::lm(y~x),stats::lm(z~x))
#'
#'
#' @export
#'
fitCompare <- function(m1, m2)
{
  a = summary(m1)$adj.r.squared
  b = summary(m2)$adj.r.squared

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
  return (list(result))
}
