

### Task 1

getwd()



### Task 2


#This will be a different path if in the lab or at home
dird="C:\\Users\\Nathan\\Downloads\\Data-for-the-course\\K25936_Downloads\\Excel\\"

#my function to read data
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
#EASY WAY TO READ IN FILES
spruce.df=myread("SPRUCE.csv")#MS pg478


#Top six lines
head(spruce.df)
#renaming column name to be the variable because of parsing error.
names(spruce.df)[names(spruce.df) == "ï..BHDiameter"] <- "BHDiameter"


### Task 3


## Scatter Plot of the Data
#initial plot, compare to get perspective ranges and intercepts
with(spruce.df,  {
layout(matrix(1:2,nr=2))
plot(Height~BHDiameter,bg="Blue",pch=21, cex=1.2)
plot(Height~BHDiameter,bg="Blue",pch=21, cex=1.2, ylim=c(0,max(Height)*1.1),xlim=c(0,max(BHDiameter)*1.1), xlab="BHDiameter", ylab="Height", main="Height vs BHDiameter")
 }
)

#There does appear to be a straight line relationship

#Loess Smooth Scatter Plot

#Load library
#make a new plot
library(s20x)
with(spruce.df,  {
  layout(matrix(1:3,nr=3))
  trendscatter(Height~BHDiameter,f=0.5, data=spruce.df, main="f=0.5")
  trendscatter(Height~BHDiameter,f=0.6, data=spruce.df, main="f=0.6")
  trendscatter(Height~BHDiameter,f=0.7, data=spruce.df, main="f=0.7")
  }
)

#Linear Model

# make a linear model
spruce.lm=with(spruce.df, lm(Height~BHDiameter))

#make a new plot
with(spruce.df,
plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
abline(spruce.lm)

#To see if the straight line is appropriate, I would look calculate a fit statistic, or the R-Squared value for the straight line.
summary(spruce.lm)
#As we can see, the straight line has an R-Squared value of .6569, low, but not too bad.  Calculating a fit statistic for loess is impossible, but it does look like the curves fit the data quite nicely.


### Task 4
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
layout.show(4)

#plot 1
with(spruce.df,
  plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))+abline(spruce.lm)
)
mtext("Scatter Plot",side=3, line= -15)

# make a linear model
spruce.lm=with(spruce.df, lm(Height~BHDiameter))

#make a new plot
with(spruce.df,
     plot(Height~BHDiameter, bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
abline(spruce.lm)


#plot 2


#make yhat the estimates of E[Height | BHDiameter]
yhat=with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
#OR you could use -- (yhat values the predicted values for all the BHDiameter values )
yhat=fitted(spruce.lm)

# Draw in segments making the residuals (regression deviations)
with(spruce.df,{
segments(BHDiameter,Height,BHDiameter,yhat)
})
abline(spruce.lm)

#residual sum of squares
RSS=with(spruce.df,sum((Height-yhat)^2))

RSS
mtext("RSS Plot",side=3, line = -15)
#make a new plot
with(spruce.df,
plot(Height~BHDiameter, bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)

#make nieve model
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)
mtext("MSS Plot",side=3, line = -15)

#make the explained deviations (explained by the model)
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS

# Total  error
#make a new plot

with(spruce.df,
plot(Height~BHDiameter, bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
mtext("TSS Plot",side=3, line = -15)
TSS=with(spruce.df,sum((Height-mean(Height))^2))
TSS


#calculate TSS, MSS, RSS
TSS
MSS
RSS

#Calculate MSS/TSS
MSS/TSS

#Does TSS=MSS+RSS?
RSS + MSS


### Task 5


summary(spruce.lm)
coef(spruce.lm)
anova(spruce.lm)
#Predict new Height values for BHDiameter values
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))


### Task 6
