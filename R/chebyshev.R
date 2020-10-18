#' Determine the number of observations within a certain standard deviation for lab 2
#'
#' More detailed description
#' @param sd number of standard deviations away from the mean
#' @param vector vector of data
#' @return the percentage of data that falls within a specified number of standard deviations from the data
#'
#' @examples
#' chebyshev(2, c(1,2,3,4,5,6,7,8,9,11,13,16,18,21))
#' chebyshev(1, c(1,2,3,11,13,16,18,21,34,41,56))
#' @export
#'
chebyshev <- function(sd, vector)
{
  z=(vector-mean(vector))/sd(vector)
  return (length(vector[abs(z)<sd])/length(vector))
}
