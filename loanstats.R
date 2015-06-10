## Lending Club Loan Stats 2015 - Final Project

## SETUP
## read in the data from the trimmed data set that Conor cleaned up
loans <- read.csv("loanstats_trimmed.csv")
state.fips <- read.csv("fips_table.csv")

## DESCRIBE DATA
# We look at a histogram of the interest rate and loan amounts
# png('int_rate_hist_w_mean.png')
hist(loans$int_rate, col="lightblue", xlab="Interest Rate", main="")
abline(v=mean(loans$int_rate),col="red")
legend(x=0.2,y=8000,c("Count","Mean = 0.129"),lty=c(1,1),col=c("lightblue","red"))
# dev.off()

# png('loan_amnt_hist_w_mean.png')
hist(loans$loan_amnt, col="lightblue", xlab="Loan Amount", main="")
abline(v=mean(loans$loan_amnt),col="red")
legend(x=22500,y=9000,c("Count","Mean = $15,300"),lty=c(1,1),col=c("lightblue","red"))
# dev.off()

# Next we plot the interest rate for loans that are almost fully drawn and those that
# are not drawn
# png('int_rate_05_w_mean.png')
hist(loans$int_rate[loans$revol_util<.05],col="lightblue", xlab="Interest Rate",main="")
abline(v=mean(loans$int_rate[loans$revol_util<.05]),col="red")
legend(x=0.18,y=225,c("Count","Mean = 0.119"),lty=c(1,1),col=c("lightblue","red"))
# dev.off()

# png('int_rate_95_w_mean.png')
hist(loans$int_rate[loans$revol_util>.95],col="lightblue", xlab="Interest Rate",main="")
abline(v=mean(loans$int_rate[loans$revol_util>.95]),col="red")
legend(x=0.18,y=600,c("Count","Mean = 0.144"),lty=c(1,1),col=c("lightblue","red"))
# dev.off()

## Look at some geo-plots of the data to see if there are any geospatial relationships
## Another method from http://www.bertplot.com/visualization/?p=524
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)
library(grid)
library(RColorBrewer)

states50 <-readOGR(dsn='cb_2013_us_state_20m',layer='cb_2013_us_state_20m')

# Change projection
states50 <- spTransform(states50, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
states50@data$id <- rownames(states50@data)

# Alaska
alaska <- states50[states50$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.2)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
# Force projection to be that of original plot
proj4string(alaska) <- proj4string(states50)

# Hawaii
hawaii <- states50[states50$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
# Force projection to be that of original plot
proj4string(hawaii) <- proj4string(states50)

# remove old Alaska/Hawaii and virgin islands, mariana, puerto rico, etc...but leave DC
states48 <- states50[!states50$STATEFP %in% c("02", "15", "72","66","60","69","74","78"),]
states.final <- rbind(states48, alaska, hawaii)

## Pull the average values by state in order to plot
loans.state.avg <- aggregate(loans,list(state=loans$addr_state),mean)
# Add the state FIPS code for plotting
fips.match <- match(loans.state.avg$state,state.fips$State.Abbreviation)
loans.state.avg$fips <- state.fips$FIPS.Code[fips.match]

# Some states are missing from the data, let's check
state.fips[is.na(match(state.fips$State.Abbreviation,loans.state.avg$state)),]
# There is no data for Iowa, Idaho, Maine, North Dakota, and Nebraska

loans.state.avg$int_rate_b <- as.integer(cut(loans.state.avg$int_rate,5))


states.final@data$state <- as.integer(states.final@data$STUSPS)
trim.data <- with(loans.state.avg, data.frame(fips=fips,state=state,int_rate_b=int_rate_b))
# Add back the missing states
add.back <- data.frame(fips=c(19,16,23,38,31),state=c("IA","ID","ME","ND","NE"),int_rate_b=c(NA,NA,NA,NA,NA))
trim.data <- rbind(trim.data,add.back)
trim.data <- trim.data[order(as.character(trim.data$state)),]
states.final@data <- merge(states.final@data,trim.data,by='state',sort=F,all.x=T)
states.plotting<-fortify(states.final,region='state')

trim.data$id <- 1:51

state.dat<-merge(states.plotting,trim.data,by='id',sort = F,all.x=T)

state.dat <- state.dat[order(state.dat$order),]

ggplot(state.dat)+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        legend.key.size = unit(x = 0.8,units = 'cm'))+
  geom_polygon(aes(x = long, y = lat,group = group,fill=int_rate_b))+
  geom_polygon(aes(x = long, y = lat,group = group),fill=NA,colour='white',size=0.5)+
  scale_fill_gradientn(colours=c("#cccccc", brewer.pal(n=5, name="YlOrRd")),na.value="#000001",
    name="Interest Rate Bucket",labels=c("11.9-12.2%","12.21-12.6%","12.61-12.9%","12.91-13.2%","13.21-13.6%"))+
  annotate("text",x=-388560.8,y=-2500000,label="*States shown in black had no borrowers in the data set")

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

