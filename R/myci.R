#' Create a 95\% confidence interval for mu given a sample normally distributed x
#'
#' More detailed description
#'
#' @param x a vector sample from a population
#'
#' @return returns a list containing the confidence interval
#'
#' @export
#'
myci<-function(x){
  v <- length(x)
  t95=qt(0.975,v)
  ci=c()
  ci[1]=mean(x)-t95*sd(x)/sqrt(v)
  ci[2]=mean(x)+t95*sd(x)/sqrt(v)
  return(list(ci=ci))
}
