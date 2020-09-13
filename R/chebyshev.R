#' Determine the number of observations within a certain standard deviation for lab 2
#'
#'  More detailed description
#'
#'  More detailed description
#' @param sd number of standard deviations away from the mean
#' @param vector vector of data
#' @return a ggplot object
#'
#' @export
#'
chebyshev <- function(sd, vector)
{
  z=(vector-mean(vector))/sd(vector)
  return (length(vector[abs(z)<sd])/length(vector))
}
