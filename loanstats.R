## Lending Club Loan Stats 2015 - Final Project

## SETUP
## read in the data
loanstats <- read.csv("loanstats.csv",skip=1)

## grab some covariates of interest
loans <- loanstats[,c(3,6,7,12,13,14,16,21,24,25,26,27,30,33,34,35,36,37,52)]
head(loans) ## see newly created data table

## check which covariates R recognizes as factors
class(loans$term) ## example - shows R recognizes "term" as a factor


## DESCRIBE DATA
hist(loans$int_rate, col="lightblue", xlab="Interest Rate", main="")
hist(loans$loan_amnt, col="lightblue", xlab="Loan Amount", main="")

plot(loans$int_rate[loans$revol_util>.95])
plot(loans$int_rate[loans$revol_util<.05])


## RUN A REGRESSION
loanslm <- glm(int_rate~., data=loans)
1 - loanslm$deviance/loanslm$null.deviance ## Calculates R-squared


## OUTLIERS
## calculate the standardized residuals for loanslm (n-k-1 = 84192) 
loanslm.sigma.hat <- (loanslm$dev/84192)**0.5
## add a column for predicted values to the loans data frame
loans$predict <- predict(loanslm)
## add a column for the residuals
loans$resid <- loans$int_rate - loans$predict
## calculate the standardized residuals
loans$std.resid <- loans$resid / loanslm.sigma.hat
## add in the outlier p-values
loans$pvals <- 2*pnorm(-abs(loans$std.resid))
## show observations with smallest p-values. These loans are outliers,
## and for some reason have an interest rate that is much different than
## what our model predicts
loans.ordered <- loans[order(loans$pvals),]
head(loans.ordered)


## FALSE DISCOVERY
## grab p-values
pvals <- summary(loanslm)$coef[-1,4] #-1 to drop the intercept
## plot them: it looks like we have some signal here
hist(pvals, xlab="p-value", main="", col="lightblue")

## At 5% FDR, we get 36 `signif'
source("fdr.R")
alpha <- fdr_cut(pvals, .05) ## cutoff
signif <- which(pvals <= alpha) ## which are significant
length(signif)  ## the number significant
names(pvals)[pvals<alpha] ## those variable significant at alpha=0.05

## Re-run a cut regression using only these 36
## [pulled from semiconductor ex - NEED TO DO DIFFERENTLY because we have factors]
# get names of variables to include
cutvar <- c("int_rate", rownames(summary(loanslm)$coef)[-1][signif]) 
# run regression on these alone
loanslm_cut <- glm(int_rate ~ ., data=loans[,cutvar]) ## DOESN'T WORK
summary(loanslm_cut)
# new in-sample R2: drops to
1 - loanslm_cut$deviance/loanslm_cut$null.deviance


## LASSO REGRESSION

library(gamlr)
source("naref.R")
mmloans <- sparse.model.matrix(int_rate ~ ., data=naref(loans))[,-1]
y <- loans$int_rate # pull out `y' too just for convenience

## note, I need lambda.min.ratio=1e-4 because otherwise we don't get a path
## out to complex enough models (i.e. cv err is still decreasing at termination)
cv.loans <- cv.gamlr(mmloans, y, lmr=1e-4, verb=TRUE )
n <- nrow(mmloans) # there are 84,277 loans in our dataset
# plot to visualize
plot(cv.loans,main="OOS R-Squared for CV Lasso")
# find the OOS R-Squared
summary(cv.loans)[cv.loans$seg.min,] # min avg OOS R2
summary(cv.loans)[cv.loans$seg.1se,] # 1se rule

## compare the AICc, AIC, and BIC selection to each other and the CV rules
summary(cv.loans$gamlr)[which.min(AIC(cv.loans$gamlr)),] # AIC
summary(cv.loans$gamlr)[which.min(AICc(cv.loans$gamlr)),] # AICc
summary(cv.loans$gamlr)[which.min(BIC(cv.loans$gamlr)),] # BIC

## plot CV results and the various IC
ll <- log(cv.loans$gamlr$lambda) ## the sequence of lambdas

plot(ll, AIC(cv.loans$gamlr)/n, xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(cv.loans$gamlr))], col="orange", lty=3)
abline(v=ll[which.min(BIC(cv.loans$gamlr))], col="green", lty=3)
abline(v=ll[which.min(AICc(cv.loans$gamlr))], col="black", lty=3)
points(ll, BIC(cv.loans$gamlr)/n, pch=21, bg="green")
points(ll, AICc(cv.loans$gamlr)/n, pch=21, bg="black")
legend("topleft", bty="n", fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))

## show CV and IC results together in a path plot
par(mfrow=c(1,1))
plot(cv.loans$gamlr, col="grey",main="Selected Model Betas vs Log Lambda")
abline(v=ll[which.min(AICc(cv.loans$gamlr))], col="black", lty=2)
abline(v=ll[which.min(AIC(cv.loans$gamlr))], col="orange", lty=2)
abline(v=ll[which.min(BIC(cv.loans$gamlr))], col="green", lty=2)
abline(v=log(cv.loans$lambda.min), col="blue", lty=2)
abline(v=log(cv.loans$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1,
  col=c("black","orange","green","blue","purple"),
  legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

## print the top three int_rate effects
print(coef(cv.loans, select="1se"))


## CAUSAL TREATMENT EFFECTS

# the naive value of beta on total credit lines
coef(cv.loans)["total_acc",] # -0.00040

## a new model matrix excluding total_acc
x <- mmloans[, -grep("total_acc",colnames(mmloans))]
## pull total_acc out as a separate vector (treatment)
d <- mmloans[, "total_acc"]

## step 1: fit the treatment regression for Total Credit Lines on x.
## said another way, predict total_acc from all other covariates
treat <- gamlr(x,d)
## grab predicted dhat from fitted betas
dhat <- predict(treat, x, type="response")
## compare d vs. dhat
plot(dhat,d,bty="n",pch=21,bg=8,main="Total Credit Lines Treatment Effect")
## calculate in sample R-squared for the d on x regression
cor(drop(dhat),d)^2

## step 2: using the double-lasso algorithm from class, we just put dhat into
## the model without any penalty (using the free argument).
## Re-run lasso to estimate independent effect of Total Credit Lines
causal <- gamlr(cBind(d,dhat,x),y,free=2)
## calculate beta
coef(causal)["d",]
## beta is -0.00049 (close to naive estimate), meaning that
## total credit lines has a causal effect.


## BOOTSTRAP

## bootstrap the lambdas using a for loop.
## note we don't need cv.gamlr here, just gamlr
B <- 50 # more samples is better, but we're not picky
bootgamma <- rep(0,B)
for(b in 1:B){
	## create a matrix of resampled indices
	ib <- sample(1:n, n,replace=TRUE)
	## create the resampled data
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	## run the treatment regression
	treatb <- gamlr(xb,db)
	dhatb <- predict(treatb,xb)
	
	fitb <- gamlr(cBind(db,dhatb,xb),yb,free=2)
	bootgamma[b] <- coef(fitb)["db",]
	print(b)
}

## plot histogram
hist(bootgamma,col=rgb(1,0,0,.5),freq=FALSE,xlim=c(-.0006,-.0004),
	ylim=c(0,20000),xlab="lambda",main="Histogram of Selected Lambdas")

