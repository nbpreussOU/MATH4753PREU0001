#' Create various plots for CLT of a poisson distribution.
#'
#' @param n the number of samples to be created for each iteration
#' @param iter the number of iterations to be created
#' @param lambda the lambda value for the poisson distribution
#' @param ... additional arguments
#' @return This function returns three graphs: a histogram of the sample mean, a theoretical normal curve, and a probability function bar plot, all with rainbow colors
#'
#' @examples
#' nathancltp(50, 1000, 6)
#' nathancltp(10, 6000)
#' nathancltp(20, 100, 2)
#' @export
#'
nathancltp=function(n,iter,lambda=10,...){
## r-random sample from the Poisson
y=stats::rpois(n*iter,lambda=lambda)
## Place these numbers into a matrix
## The columns will correspond to the iteration and the rows will equal the sample size n
data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
## apply the function mean to the columns (2) of the matrix
## these are placed in a vector w
w=apply(data,2,mean)
## We will make a histogram of the values in w
## How high should we make y axis?
## All the values used to make a histogram are placed in param (nothing is plotted yet)
param=graphics::hist(w,plot=FALSE)
## Since the histogram will be a density plot we will find the max density

ymax=max(param$density)
## To be on the safe side we will add 10% more to this
ymax=1.1*ymax

## Make a suitable layout for graphing
graphics::layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

## Now we can make the histogram
graphics::hist(w,freq=FALSE,  ylim=c(0,ymax), col=grDevices::rainbow(max(w)),
     main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
     xlab="Sample mean",...)
## add a density curve made from the sample distribution
#lines(density(w),col="Blue",lwd=3) # add a density plot
## Add a theoretical normal curve
graphics::curve(stats::dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

# Now make a new plot
# Since y is discrete we should use a barplot
graphics::barplot(table(y)/(n*iter),col=grDevices::rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
x=0:max(y)
plot(x,stats::dpois(x,lambda=lambda),type="h",lwd=5,col=grDevices::rainbow(max(y)),
     main="Probability function for Poisson", ylab="Probability",xlab="y")
}
