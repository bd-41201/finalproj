## currency codes:

fx <- read.csv("FXmonthly.csv")
## note: these are amount in local currency to buy 1 USD.
## so high is stronger relative USD.
## They were options so not the true historic prices.
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,]) # proportion change

## [1] 
## look at some correlations; very high multicollinearity
cor(fx[,c('exeuus','exhkus','excaus','exmxus','exukus')])
## There is very high correlation amongst x.
## this indicates that we might be able to re-represent it
## in a low-D factor structure

## [2] PCAs 
plot(fxpca <- prcomp(fx, scale=TRUE), main="")
mtext(side=1, "Currency Difference Principle Components",  line=1, font=2)
fxdir <- predict(fxpca)
plot(fxdir[,1:2], pch=21, bg=terrain.colors(120)[120:1], main="% change in rates")
legend("topleft", fill=terrain.colors(3), legend=c("2010","2005","2001"), bty="n")

## read returns for regression
SP <- as.matrix(read.csv("sp500.csv",row.names=1))

## consider the first 3 principle components
summary(sp500pcr <- lm(SP ~ ., data=data.frame(fxdir[,1:3])))
plot(SP, sp500pcr$fitted, pch=21, bg=terrain.colors(120)[120:1],
     xlab="SP500 monthly returns", ylab="fitted values")
## there is a big outlier on lehman tanking
out <- which.min(sp500pcr$fitted)
text(x=SP[out]+.01, y=sp500pcr$fitted[out]+.01, labels=rownames(SP)[out], cex=.75)

## we could re-run everything without the outlier (fine if you didn't do this)
SP <- SP[-out]
fx <- fx[-out,]
fxpca <- prcomp(fx, scale=TRUE)
fxdir <- as.data.frame(predict(fxpca))

## The first PC appears to be US dollar dropping (near every loading is negative)
## The second looks like current account deficit perhaps?
## Third could be volatility/uncertainty...
## Its not clear though.  Some of you probably have better stories!
round(fxpca$rotation[,1:3]*10)

## plot on the first two PC dimensions, colored by cluster for fun

## do some k-means, for comparison
grpFX <- kmeans(scale(fx), centers=10, nstart=20)
plot(fxdir[,1:2], type="n")
text(x=fxdir[,1], y=fxdir[,2], labels=rownames(fx), col=rainbow(10)[grpFX$cluster])

## [3] PC Regression

library(gamlr) 
## Get glm fits on 1:20 factors
kfits <- lapply(1:20, # do the below for K=1:20
	function(K) glm(SP~., data=fxdir[,1:K,drop=FALSE]))
## Both AICc and BIC like three factors
aicc <- sapply(kfits, AICc) 
which.min(aicc)
bic <- sapply(kfits, BIC) 
which.min(bic) 

## plot 'em 
plot(aicc, pch=21, bg="maroon", xlab="K", ylab="AICc")

## looks like 3 is best, so use that
summary(sp500pcr <- glm(SP ~ ., data=fxdir[,1:3]))
plot(SP, sp500pcr$fitted, pch=21, bg=terrain.colors(120)[120:1],
     xlab="SP500 monthly returns", ylab="fitted values")

## re-interpret
summary(sp500pcr)
## 1st is rising S&P; not obvious because this is priced in USD and the 1st PC was USD strength.
## 2nd is big drop in S&P, which perhaps make sense with our 'commodities' interpretation?
## It is hard to tell without some economics though...


## alternatively, try the lasso
lassoPCR <- cv.gamlr(x=fxdir, y=SP)
plot(lassoPCR) 
## it agrees on the need for those first three
## but adds in some others
coef(lassoPCR) 
## the fact that we've loaded on PC23 (the last one!) 
## is a bit strange.  This doesn't normally happen...
fxpca$rotation[,23]
# it is only big on denmark, and big with opposite sign on the EU
# Thus the 23rd PC direction scores big whenever EU and DN move away from each other.
#
# this is strange because denmark is pegged to euro!
plot(fx[,c("exdnus","exeuus")]) # close, but not perfect
# the peg uses ERMII: http://en.wikipedia.org/wiki/European_Exchange_Rate_Mechanism
# So DN and EU only move apart when the fed exchange rate disagrees with the ERMII formula.
# My guess: this happens when the market is increasing quickly, hence the positive coefficient.

## [4] straight up lasso; builds an interesting model model.  
## Not sure why sweden shows up; a euro currency independent from the actual euro?
## And australia?  A nearly-independent commodity indicator?
fxlasso <- cv.gamlr(x=fx, y=SP, nfolds=5)
plot(fxlasso)
coef(fxlasso)


## [+] marginal regression and PLS
library(textir)
margfit <- pls(fx,SP,K=1)

##  Marginal regression just gets PC1!
plot(margfit$fitted, fxdir[,1]) 
## so because PC1 was driven by the US economy, which also drives SP500,
## here the dominant source of variation in x is closely related to y.

# look at higher order PLS
fxpls <- pls(fx,SP,K=4)
# don't gain that much R2 with a few factors; 
# it's not like the gas example
plot(fxpls, pch=21, bg=8) 
