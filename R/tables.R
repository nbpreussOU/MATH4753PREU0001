### READING IN DATA FROM A CSV###
getwd()
ddt <- read.csv("DDT.csv")
head(ddt)


### USING FAKE DATA ###
set.seed(23)
l1 =rnorm(30, 40, 5)
l2 =rnorm(35, 50, 5)
l3 =rnorm(65, 60, 10)
ll =c(l1,l2)
LL =rep(c("Sparrow", "Bat"),c(30,35))
df =data.frame(Bird = LL, Length = ll, Temp = l3)
head(df)


### SLICING DATA BY CREATING A NEW DATA FRAME ###

bats <- df[df$Bird=="Bat", ] #creates a data frame with a rows containing bats, and taking all of the columns
head(bats) # This is a data frame (2 columns)

bats2 <- df[df$Bird=="Bat", "Length"]# could use 2 for Length, only takes certain column,  SELECT * FROM LENGTH WHERE BIRD = BATS
head(bats2) # this is a vector (one column)

#what is the probability that a species will weigh more than 800 given that is is LMBASS P(Weight > 800 | Species = LMBASS)
with(ddt, ddt[WEIGHT>800 & SPECIES=="LMBASS",])
with(ddt, ddt[SPECIES == "LMBASS",])

### Factoring Data ###
unique(df[1])
unique(ddt[3])

df2 <- df
df2$Bird <-factor(df$Bird, levels=c("Sparrow", "Bat"))
summary(df2)

ddt2 <- ddt
ddt2$SPECIES <- factor(ddt$SPECIES)
ddt2$RIVER <- factor(ddt$RIVER)
summary(ddt2)


### VECTORS FROM DATAFRAMES ###

ddt.vector <- as.vector(ddt2$DDT)
# calculating z score
z=(ddt.vector-mean(ddt.vector))/sd(ddt.vector) # z is a vector
ddt.vector[abs(z)>=2 & abs(z)<=3] # in this case there are no possible outliers
ddt.vector[abs(z)>3] # shows the ddt levels of the outliers

ddtLMBASS <- as.vector(ddt2[ddt2$SPECIES == "LMBASS", 6]) # a vector of ddt levels where species equals LMBASS
z1 = (ddtLMBASS - mean(ddtLMBASS))/sd(ddtLMBASS) # calculate the z score
ddtLMBASS[abs(z1)>=2] # find all outliers and potential outliers
with(ddt2, ddt2[ddt2$SPECIES == "LMBASS", 6]) # verify by looking at the section of data


### USING TABLES ###

#another way to slice the data by species P(SPECIES==LMBASS | weight > 800)
table(ddt2[ddt2$WEIGHT>800, 3])
table(ddt2$SPECIES)

# P(SPECIES = SMBUFFALO | RIVER == TRM)
table(ddt2[ddt2$RIVER == "TRM", 3])
table(ddt$RIVER)


### SUMMARY LM OBJECT ###
y = ddt2$LENGTH
x = ddt2$WEIGHT
model=lm(y~x) # model comparing length and weight
summary(model)

#beta 1
summary(model$residuals) # calculate mean of residuals (should be 0)
ssxx = sum((x-mean(x))*x)
ssxy = sum((x-mean(x))*y)
beta1h = ssxy/ssxx
beta1h # beta 1 hat

#beta 0
beta0h = mean(y) - beta1h*mean(x)
beta0h # beta 0 hat

#standard error beta 0
rss = sum(model$residuals^2)
ssq = rss/(length(x)-2) # n-2
n = length(x)
s = sqrt(ssq)
stderrb0h = s*sqrt( sum(x^2)/(n * ssxx))
stderrb0h

#standard error beta 1
stderrb1h = s/sqrt(ssxx)
stderrb1h

#tcalc intercept
tcalcb0 = beta0h/stderrb0h
tcalcb0

#tcalc beta 1
tcalcb1 = beta1h/stderrb1h
tcalcb1

#pvalues
alpha = 0.05
2*(1-pt(abs(tcalcb0),n-2)) #beta 0
(1-pt(abs(tcalcb1),n-2))*2 #beta 1



#Residual Standard error (Like Standard Deviation)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
r=length(model$residuals)
sqrt(SSE/(r-(1+k))) #Residual Standard Error

#Multiple R-Squared (Coefficient of Determination)
SSyy=sum((y-mean(y))^2)
r2 = (SSyy-SSE)/SSyy
r2

#F-Statistic
#Ho: All coefficients are zero
#Ha: At least one coefficient is nonzero
#Compare test statistic to F Distribution table
((SSyy-SSE)/k) / (SSE/(n-2))


### Confidence interval and predictions ###
pdct <- predict(model, newdata = data.frame(x=c(19,25)), interval = "prediction")
pdct

ci <- predict(model, newdata = data.frame(x=c(19,25)), interval = "confidence")
ci

