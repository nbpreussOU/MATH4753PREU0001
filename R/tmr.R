#' Determine TSS, MSS, and RSS for lab 3
#'
#'  More detailed description
#'
#' @param y the y variable of the model
#' @param x the x variable of the model
#' @param dataArg the dataframe for the model
#'
#' @return returns a vector with value[1] = TSS, value[2] = MSS, value[3] = RSS
#'
#'
#' @export
#'
#'
#'
tmr <- function(y, x, dataArg)
{
  lm.lm <- lm(y~x, data=dataArg)
  yhat=fitted(lm.lm)
  RSS = sum((y-yhat)^2)
  MSS = sum((yhat-mean(y))^2)
  TSS = sum((y-mean(y))^2)

  return (c(TSS, MSS, RSS))
}
