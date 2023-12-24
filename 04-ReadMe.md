#regression and R^2
SC <- read.csv("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance
##[1] 0.5621432

# Step 1 - K-fold functions
##first, define deviance and R^2 functions (calculating gaussian deviance if family is gaussian distribution, otherwise (binomial) deviance using log likelihood)
deviance <- function(y, pred, family=c("gaussian","binomial")){
	family <- match.arg(family)
	if(family=="gaussian"){
		return( sum( (y-pred)^2 ) )
	}else{
		if(is.factor(y)) y <- as.numeric(y)>1
		return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
	}
}

## get null devaince too, and return R2 (translating y to numeric vector if it's binomial, returning R^2)
R2 <- function(y, pred, family=c("gaussian","binomial")){
	fam <- match.arg(family)
	if(fam=="binomial"){
		if(is.factor(y)){ y <- as.numeric(y)>1 }
	}
	dev <- deviance(y, pred, family=fam)
	dev0 <- deviance(y, mean(y), family=fam)
	return(1-dev/dev0)
}

# Step 2 - K-fold Partition/Experiment
# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
	train <- which(foldid!=k) # train on all but fold `k'
		
	## fit regression on full sample
	rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)

	## get prediction: type=response so we have probabilities
	predfull <- predict(rfull, newdata=SC[-train,], type="response")

	## calculate and log R2
	Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")

	## print progress
	cat(k, " ")
}

# Step 3 - K-fold plots (negative R^2, due to Dev being bigger than Dev0. Thus, the fit of the null model is better than the fitted model)
boxplot(Out, col="plum", ylab="R2")


##EXPERIMENT 1: MAKING R^2 BOXPLOT FOR EACH FOLD (there are 10 folds and the plots look very similar. therefore, it's worth investigating whether the model is actually stable or if there's an issue in the cross-validation setup. also, R^2 is still negative)
par(mfrow=c(2,5)) 
for (k in 1:K) {
  boxplot(Out[, "full"], col="plum", ylab=paste("R2 - Fold ", k))
}

# Forward Stepwise Regression (forward stepwise model selection based on the null model. The scope argument specifies the set of models to be considered in the selection process, using regression from before. )
null <- glm(FAIL~1, data=SC) 
fwd <- step(null, scope=formula(full), dir="forward")
length(coef(fwd))
##[1] 69, thus final model has 69 coefficients

#EXPERIMENT 2: DOING BACKWARD STEPWISE REGRESSION INSTEAD. HOW MANY COEFFICIENTS DOES THE FINAL MODEL HAVE?
# Backward stepwise selection
null <- glm(FAIL~1, data=SC) 
bwd <- step(null, scope=formula(full), dir="backward")
length(coef(bwd))
##[1] 1, thus with backward stepwise regression, only 1 coefficient. does not sound reliable

#LASSO regularization path: algo 6
install.packages("gamlr")
rm(list = ls())
library(gamlr)
## Browsing History. 
## web has 3 colums: [machine] id, site [id], [# of] visits
web <- read.csv("/Users/robina/Desktop/DSHRA Weekly/04-regularization/browser-domains.csv")
## Read in actual website names and relabel site factor
sitenames <- scan("/Users/robina/Desktop/DSHRA Weekly/04-regularization/browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))
## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]
## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
	i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
	dims=c(nlevels(web$id),nlevels(web$site)),
	dimnames=list(id=levels(web$id), site=levels(web$site)))

## now read in the spending data (result, path diagram with log lamda on x axis, striped line through 226)
yspend <- read.csv("/Users/robina/Desktop/DSHRA Weekly/04-regularization/browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix
spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
plot(spender)

# K-fold Cross Validation for LASSO (adjusted margins as it would not plot) (shows minimum MSE around a og lamda of -3.75, which means that that is the optimal penalization)
cv.spender <- cv.gamlr(xweb, log(yspend))
par(mar = c(4, 5, 2, 2))  
plot(cv.spender)
betamin = coef(cv.spender, select="min"); betamin

#EXPERIMENT 3: Visualizing Coefficient Paths (very similar to normal path plot, so I do not think this makes a difference)
library(gamlr)
# Fit and plot the model with coefficient paths
spender_paths <- gamlr(xweb, log(yspend), paths = TRUE)
par(mfrow=c(1,1), mar=c(5, 5, 4, 2))
plot(spender_paths, main = "Coefficient Paths for Different Lambda Values")

#EXPERIMENT 4: Comparing LASSO with Ridge Regression
# Fit LASSO model
spender_lasso <- gamlr(xweb, log(yspend), alpha = 1)
# Fit Ridge model
spender_ridge <- gamlr(xweb, log(yspend), alpha = 0)
# Compare model performance (e.g., cross-validated MSE)
cv_lasso <- cv.gamlr(xweb, log(yspend), alpha = 1)
cv_ridge <- cv.gamlr(xweb, log(yspend), alpha = 0)
# Display cross-validated MSE for comparison
cv_lasso$cvm
cv_ridge$cvm
##all coefficients are slightly different, but still very very similar



