## ----libraries-----------------------------------------------------------
library(knitr)
library(doBy)
library(msm)


## ----load data-----------------------------------------------------------
load(url("http://people.stat.sc.edu/hoyen/Stat704/Data/h129.RData"))
dat = data.frame(h129$TOTEXP09, h129$SEX, h129$RACEX, h129$ASTHDX, h129$DOBMM, h129$DOBYY,h129$ADSMOK42, h129$CALUNG, h129$CHDDX, h129$EDUCYR, h129$POVCAT09, h129$MARRY09X, h129$SEATBE53, h129$PERWT09F, h129$AGE09X)



## ----expenditures plot---------------------------------------------------
plot.expenditures=NULL

# find and store expenditures near 10,000
for(i in 1:nrow(dat)){
  ifelse (dat$h129.TOTEXP09[i]>10000, plot.expenditures[i] <- 10001, plot.expenditures[i]<-dat$h129.TOTEXP09[i]) 
}

# plot expenditures vs. age
plot(jitter(dat$h129.AGE09X,2),plot.expenditures, cex = .25, main="Expenditures vs. Age", 
xlab = "Age", ylab = "Expenditures", col = rgb(0.3,0.3,0.3, 0.4), pch = 16)
# add the lowess smoothing
lines(lowess(dat$h129.AGE09X,dat$h129.TOTEXP09), lwd = 2, col = "red")



## ----spline at 40--------------------------------------------------------
age40 = ifelse(dat$h129.AGE09X > 40, dat$h129.AGE09X-40, 0)
fitlsp = lm(dat$h129.TOTEXP09 ~ dat$h129.AGE09X + age40)
summary(fitlsp)


## ----expenditures by gender----------------------------------------------

# Separating men and women
male.data = dat[which(dat$h129.SEX == 1),]
female.data = dat[which(dat$h129.SEX ==2),]

# plot expenditures vs. age
plot(jitter(dat$h129.AGE09X,2),plot.expenditures, cex = .25, main="Expenditures vs. Age", 
xlab = "Age", ylab = "Expenditures", col = rgb(0.3,0.3,0.3, 0.4), pch = 16)

# add the lowess smoothing

# mixed
lines(lowess(dat$h129.AGE09X,dat$h129.TOTEXP09), lwd = 2, col = "red")
# for men
lines(lowess(male.data$h129.AGE09X,male.data$h129.TOTEXP09), lwd = 2.5, lty = 2, col = "blue")
# for women
lines(lowess(female.data$h129.AGE09X,female.data$h129.TOTEXP09), lwd = 2.5, lty = 3, col = "green")
# add the legend
legend("topleft", c("Mixed", "Male", "Female"), col=c("red", "blue", "green"), lty=c(1,2,3))



## ----gender spline-------------------------------------------------------
dat$h129.SEX = as.factor(dat$h129.SEX)
# add gender as a spline interaction term
MLR.gSpline = lm(dat$h129.TOTEXP09 ~ dat$h129.AGE09X + age40 + dat$h129.SEX + dat$h129.AGE09X*dat$h129.SEX + age40*dat$h129.SEX)
summary(MLR.gSpline)


## ----resid, qq, box plots------------------------------------------------
#Using the MLR line with the spline and interaction terms
MLRresid = resid(MLR.gSpline)
plot(jitter(dat$h129.AGE09X), MLRresid,  cex = .25, ylim = c(-20000,20000),
  ylab="Residuals", xlab="Age", 
   main="Expenditures and Age Residuals", col = rgb(0.3,0.3,0.3, 0.4), pch = 16) 
abline(0, 0, col = "red")

# standardized residuals
stand.resid = rstandard(MLR.gSpline)
plot(jitter(dat$h129.AGE09X), stand.resid,  cex = .25, ylim = c(-10,10),
  ylab="Residuals", xlab="Age", 
   main="Standardized Residuals", col = rgb(0.3,0.3,0.3, 0.4), pch = 16) 
abline(0, 0, col = "red")  

#QQ Plot using full data
qqplot(dat$h129.AGE09X,dat$h129.TOTEXP09, xlab = "Age",
       ylab = "Expenditures", main = "QQPlot")
qqline(dat$h129.AGE09X, datax = T, col = "red")

#Standardized Resid QQ Plot
qqplot(dat$h129.AGE09X,stand.resid, xlab="Age", ylab="Standardized Residuals", 
       main = "Standardized QQ plot")
qqline(dat$h129.AGE09X, datax = T, col = "red")

# plot residuals v fitted and normal Q-Q
plot(MLR.gSpline, which = c(1,2))

# check for outliers
boxplot(MLRresid, main = "Boxplot of Residuals") 



## ------------------------------------------------------------------------
lexp = log10(dat$h129.TOTEXP09 + 100)
plot(jitter(dat$h129.AGE09X),lexp, xlab = "Age", ylab = "Log(Expenditures)",
     main = "Log-Medical Expenditures by Age",
     cex=0.25, col = rgb(0.3,0.3,0.3, 0.4), pch = 16)
lines(lowess(dat$h129.AGE09X,lexp), lwd = 2, col= "red")

# let's try the splines, we see a 
# significant change in slope in the lowess line around 30

age30 = ifelse(dat$h129.AGE09X > 30, dat$h129.AGE09X-30, 0)


LogModel.fit = lm(lexp ~ dat$h129.AGE09X + age30 + dat$h129.SEX + 
                    dat$h129.AGE09X*dat$h129.SEX + age30*dat$h129.SEX)
summary(LogModel.fit)




## ----hypothesis testing--------------------------------------------------
fitFull = lm(lexp ~ dat$h129.AGE09X + age30 + dat$h129.SEX + 
               dat$h129.AGE09X*dat$h129.SEX + age30*dat$h129.SEX)
# reduced is full w/o interaction terms
fitRed = lm(lexp ~ dat$h129.AGE09X + age30 + dat$h129.SEX) 
ftest = anova(fitRed,fitFull)
ftest


## ----diff in logexp by gender, age---------------------------------------
wmdif = c()
# -1 : 85 are ages
for(n in -1:85){
  lambda = c(0,0,0,1,n,ifelse(n>30,n-30,0))
  z = esticon(LogModel.fit, lambda)
  wmdif = c(wmdif,z$Estimate)
}
plot(-1:85, wmdif, xlab = "Age", ylab="Log(Exp) ", main="Difference in Log- Exp (Women-Men) vs. Age", pch = 16)
lines(-1:85, wmdif, col = "red")


## ----age dfbetas---------------------------------------------------------
fit = lm(lexp ~ dat$h129.AGE09X + age30 + dat$h129.SEX 
         + dat$h129.AGE09X*dat$h129.SEX + age30*dat$h129.SEX)

# fit the dfbetas
fit.dfbetas = dfbetas(fit)

# set our cutoff
cutoff = 2 / sqrt(nrow(dat))

# now, which ones are outliers
outlierPos = which(fit.dfbetas[,2]>cutoff)
outlierNeg = which(fit.dfbetas[,2] < -1*cutoff)
outliers = c(outlierPos,outlierNeg)
outlier.dat = dat[outliers,]

# plot the influential points based on size
size = 33 * abs(fit.dfbetas[outliers,2])

plot(outlier.dat$h129.AGE09X,lexp[outliers], pch = 19, xlab= "Age", ylab="Log(Expenditure)", main = "DFBetas Plot Showing Influential Points", col = rgb(0.5,0.3,1, 0.8), cex = size)

points(jitter(dat$h129.AGE09X[-outliers]),lexp[-outliers], xlab = "Age", pch = 16, cex = 0.25, col = rgb(0.3,0.3,0.3, 0.4))



## ----plot w, w.o. outliers-----------------------------------------------
newdata = dat[-outliers,]

# plot the new data for an idea on spline location
plot(jitter(newdata$h129.AGE09X,2),lexp[-outliers], cex = .25, main="Expenditures vs. Age", 
     xlab = "Age", ylab = "Log(Expenditures)", col = rgb(0.3,0.3,0.3, 0.4), pch = 16)
lines(lowess(newdata$h129.AGE09X,lexp[-outliers]), lwd = 2, col = "red")
lines(lowess(dat$h129.AGE09X,lexp), lwd = 1.5, lty = 4 , col= "blue")
legend("topleft", c("Without outliers", "With outliers"), col=c("red", "blue"), lty=c(1,4))


## ----lm w.o. outliers----------------------------------------------------
f.age30 = ifelse(newdata$h129.AGE09X > 30, newdata$h129.AGE09X-30, 0)

NoOutlierFit = lm(lexp[-outliers] ~ newdata$h129.AGE09X + f.age30 + newdata$h129.SEX + newdata$h129.AGE09X*newdata$h129.SEX + f.age30 * newdata$h129.SEX)
summary(NoOutlierFit)


## ----difference and matrix-----------------------------------------------

# compute difference in expenditures (women - men)
difference = function(Age){
  lambda = c(0,0,0,1,Age,Age-30)
  results = esticon(NoOutlierFit, lambda)
  return(results)
}

# Create a matrix for kable, later
lexpend.mat = matrix(NA,3,3)
colnames(lexpend.mat) = c("Difference in Lexpend", "Std Error", "95% CI")
rownames(lexpend.mat) = c("40","65","80")

# Estimated diff
lexpend.mat[1, 1] = difference(40)[1,1]  # extract the estimate from diff
# SE
lexpend.mat[1,2] = difference(40)[1,2] # extract the SE from diff


CI1 = as.character(paste("(", round(difference(40)[1,7],digits = 4), round(difference(40)[1,8], digits = 4), ")", sep = "  "))
lexpend.mat[1, 3] = CI1

# Estimated diff
lexpend.mat[2, 1] = difference(65)[1,1] 
# SE
lexpend.mat[2,2] = difference(65)[1,2] #Std Error


CI2 = as.character(paste("(", round(difference(65)[1,7],digits = 4), round(difference(65)[1,8], digits = 4), ")", sep = "  "))
lexpend.mat[2, 3] = CI2

# Estimated diff
lexpend.mat[3, 1] = difference(80)[1,1] 
# SE
lexpend.mat[3,2] = difference(80)[1,2] 

CI3 = as.character(paste("(", round(difference(80)[1,7],digits = 4), round(difference(80)[1,8], digits = 4), ")", sep = " "))
lexpend.mat[3, 3] = CI3

# Round all entries
for(i in 1:2){
  for(j in 1:3){
    lexpend.mat[j,i] = round(as.numeric(lexpend.mat[j,i]),digits = 4)
  }}

kable(lexpend.mat)



## ----kable calculations--------------------------------------------------
# Matrix to be used later for kable
MedEx.mat = matrix(NA,3,4)
colnames(MedEx.mat) = c("Median Diff", "Std Error (Delta)", "Std Error (bootstrap)", "95% CI")
rownames(MedEx.mat) = c("40","65","80")

############################################################
# follow same lambda idea as earlier
# where we have 1 for intercept, age, spline, etc.
#And a median estimate for men at a certain age and then subract
lambda = c(1,40,10,1,40,10) 
women40 = esticon(NoOutlierFit, lambda)$estimate

lambda2 = c(1,40,10,0,0,0)
men40 = esticon(NoOutlierFit, lambda2)$estimate

MedEx.mat[1,1] = round(10^(women40) - 10^(men40), digits = 4)
 
lambda = c(1,65,35,1,65,35)
women65 = esticon(NoOutlierFit, lambda)$estimate

lambda2 = c(1,65,35,0,0,0)
men65 = esticon(NoOutlierFit, lambda2)$estimate

MedEx.mat[2,1] = round(10^(women65) - 10^(men65), digits = 4)
  
lambda = c(1,80,50,1,80,50)
women80 = esticon(NoOutlierFit, lambda)$estimate

lambda2 = c(1,80,50,0,0,0)
men80 = esticon(NoOutlierFit, lambda2)$estimate

MedEx.mat[3,1] = round(10^(women80) - 10^(men80), digits = 4)

############################################################

#Delta method to get Std Error
f.age30 = ifelse(newdata$h129.AGE09X > 30, newdata$h129.AGE09X-30, 0)

SansOutlierFit =  lm(lexp[-outliers] ~ newdata$h129.AGE09X + f.age30 + newdata$h129.SEX + newdata$h129.AGE09X*newdata$h129.SEX + f.age30*newdata$h129.SEX)

MedEx.mat[1,2] = round(deltamethod(~ 10^(x1+40*x2+10*x3+x4+40*x5+10*x6)
                                                  -10^(x1+40*x2+10*x3), coef(NoOutlierFit), vcov(NoOutlierFit)), digits = 4)

MedEx.mat[2,2] = round(deltamethod(~ 10^(x1+65*x2+35*x3+x4+65*x5+35*x6)
                                                 -10^(x1+65*x2+35*x3), coef(NoOutlierFit), vcov(NoOutlierFit)), digits = 4)

MedEx.mat[3,2] = round(deltamethod(~ 10^(x1+80*x2+50*x3+x4+80*x5+50*x6)
                                                 -10^(x1+80*x2+50*x3), coef(NoOutlierFit), vcov(NoOutlierFit)), digits = 4)

############################################################

# easier to make a new mat for the bootstrap than use the kable one
newmat = cbind(newdata,lexp[-outliers],f.age30)
colnames(newmat)[16] = "lexp"
colnames(newmat)[2] = "sex"
colnames(newmat)[15] = "age"

# bootstrap function
boots = function(data, nboots=1000, age){
  diff = rep(NA,nboots)
  data[,"sex"] <- as.factor(data[,"sex"])
  for(i in 1:nboots){
    sam = sample(1:nrow(data), nrow(data), replace =T)
    fit = lm(lexp ~ age + f.age30 + sex + age*sex + f.age30*sex, data = data[sam,])
    a = round(predict(fit,newdata=data.frame(age=age, sex=as.factor(2), f.age30=ifelse(age>30,age-30,0))), digits = 4)
    
    b = round(predict(fit,newdata=data.frame(age=age, sex=as.factor(1), f.age30=ifelse(age>30,age-30,0))), digits = 4)
    diff[i]=round(10^a-10^b, digits = 4)
  }
  return(list(sd = sd(diff),lower = round(quantile(diff,0.025), digits = 4),upper = round(quantile(diff,1-0.025), digits = 4)))
}

b40 = boots(newmat,1000,40)
b65 = boots(newmat,1000,65)
b80 = boots(newmat,1000,80)

MedEx.mat[,3] = c(b40$sd,b65$sd,b80$sd)
MedEx.mat[,4] = c(paste(b40$lower,",",b40$upper), paste(b65$lower,",",b65$upper), paste(b80$lower,",",b80$upper))

kable(MedEx.mat)



## ----final model summary-------------------------------------------------
summary(NoOutlierFit)


## ----mult linear regression matrix---------------------------------------
multiple.regression = function(x, y) {
  
 # Matrix of feature variables from data
  X = as.matrix(x)

  # vector of ones with same length as rows in data
  intercept = rep(1, length(y))

  # Add intercept column to X
  X = cbind(intercept, X)
  
  # find betas
  betas = solve(t(X) %*% X) %*% t(X) %*% y

  # Round for easier viewing
  betas = round(betas, 4)
  
  return(betas)
}

multiple.regression(lexp, dat$h129.AGE09X)


