#' Determine the difference between a binomial sample and dbinom
#' Modified version of mybin from the lab5 help files
#'
#' @param iter number of data points in the sample
#' @param n number of success
#' @param p probability (0 < p < 1)
#' @return a dataframe table showing the sample distribution, the theoretical distribution, and the difference between the two
#'
#' @examples
#' bindif()
#' bindif(iter=10000, n=8, p=0.2)
#' bindif(iter = 500)
#' bindif(n=7)
#' bindif(p=0.5)
#' bindif(n=8, p=0.2)
#' @export
#'
bindif=function(iter=10000,n=10, p=0.7){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))

  # create vectors to turn into rows of the dataframe
  x = c(succ.tab/iter)
  y = c()
  z = c()
  for(j in 0:n)
  {
    y <- c(y, stats::dbinom(j, n, p))
    z <- c(z, j)
  }
  dif = x-y

  #creating the finished dataframe in a somewhat inefficient, roundabout manner
  df = as.data.frame(t(x))
  df2 = as.data.frame((t(y)))
  df3 = as.data.frame(t(dif))
  colnames(df) <- z
  colnames(df2) <- z
  colnames(df3) <- z
  dbin <- rbind(df, df2, df3)
  dbin[,] <-round(dbin[,],5)
  dbin <- data.frame(name = c("sample", "dbinom", "difference"), dbin)
  dbin
}
