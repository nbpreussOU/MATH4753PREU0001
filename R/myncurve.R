#' Print out a curve like in lab 6.
#' display the curve, shaded area between the curve and x axis from to x=a, and
#' calculate the area ??????(probability, P(X<=a)) which is released to the command line
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a x value for the graph such that the graph will display values -inf < x < a
#' @return a graph
#'
#' @export
#'
#'
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma),   xlim=c(mu-3*sigma,mu+3*sigma))
  xcurve=seq(mu-3*sigma, a, length=1000)
  ycurve=dnorm(xcurve, mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a, mu, sigma)
  prob=round(prob,4)
  print(paste0("Area is ", prob))
}
