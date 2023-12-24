# Example - Estimating consumer demand elasticities 
oj <- read.csv("/Users/robina/Desktop/DSHRA Weekly/03-regression/oj.csv", header = TRUE)
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)
## (Intercept)  log(price) 
##  10.423422   -1.601307 --> Cannot interpert this as causal

#EXPERIMENT 1: elasticities for each brand
brand_coefs <- coef(brandfit)
elasticities <- brand_coefs["log(price)"]
print(elasticities)
##log(price) 
## -3.138691 --> same as before, do does not work

#EXPERIMENT 2: is the log price significant?
t_test_result <- summary(brandfit)$coefficients["log(price)", c("t value", "Pr(>|t|)")]
print(t_test_result)
##  t value  Pr(>|t|) 
##-136.8881    0.0000  --> p>0.001 so yes, significant

brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)
##     (Intercept) brandminute.maid   brandtropicana       log(price) 
##      10.8288216        0.8701747        1.5299428       -3.1386914 --> controlling for brand implies stronger elasticity

#EXPERIMENT 3: Comparing models using likelihood ratio test
install.packages("lmtest")
library(lmtest)
lr_test <- lrtest(basefit, brandfit)
print(lr_test)
##model 1  -38251                        
##model 2  -34377

# Example - Estimating consumer demand elasticities 
pricereg <- lm(log(sales) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj) 
presid <- log(oj$price) - phat
residfit <- lm(log(sales) ~ presid, data=oj)
coef(basefit)

#LTE
data <- read.table("/Users/robina/Desktop/DSHRA Weekly/06-controls/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]
dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]
exp(dcoef) - 1

#abortion rates
cell <- read.csv("/Users/robina/Desktop/DSHRA Weekly/06-controls/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")

#example
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1

t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]

#back to abortion example
library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)
## naive lasso regression
naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",] 
##[1] -0.1006174

treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)
##[1] 624

par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3))  
##looks like a strong relation, very bottom-heavy though

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 indicates how much independent signal you have for estimating 
coef(summary( glm( y ~ d + dhat) ))
# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] 
##--> 0
# AICc says abortion rate has no causal effect on crime.

#Uncertainty quantification with lasso regressions
library(gamlr)
data(hockey)
head(goal, n=2)
player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice. 
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.

#sample splitting example
x <- cbind(config,team,player)
y <- goal$homegoal
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
##[1] 2 1 1 2 2 2
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
	free=1:(ncol(config)+ncol(team)), 
	family="binomial", standardize=FALSE)
selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
			subset=which(fold==2), family=binomial )
			
#standard errors
summary(nhlmle)

x[1,x[1,]!=0] #check first observation for players on the ice
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
CI = fit + c(-2,2)*se.fit
CI #90% confidence interval for probability that Edmonton scored the goal is 
##[1] 0.2818377 0.6104710

# Orthogonal ML for LTE
library(Matrix)
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd
cell <- read.csv("/Users/robina/Desktop/DSHRA Weekly/06-controls/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
phone <- cellrate[ t + 1 ]
t <- factor(t)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]

install.packages("AER")
library(AER)
library(gamlr)

dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }


# Orthogonal ML R Function

orthoLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
	# randomly split data into folds
	nobs <- nrow(x)
    foldid <- rep.int(1:nfold, 
    	times = ceiling(nobs/nfold))[sample.int(nobs)]
    I <- split(1:nobs, foldid)
    # create residualized objects to fill
	ytil <- dtil <- rep(NA, nobs)
	# run OOS orthogonalizations
	cat("fold: ")
	for(b in 1:length(I)){
		dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
		yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
		dhat <- predict(dfit, x[I[[b]],], type="response")
		yhat <- predict(yfit, x[I[[b]],], type="response")
		dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
		ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
		cat(b," ")
	}
	rfit <- lm(ytil ~ dtil)
	gam <- coef(rfit)[2]
	se <- sqrt(vcovHC(rfit)[2,2])
	cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))

	return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}

#EXPERIMENT 4: 3 folds
orthoLTE <- function(x, d, y, dreg, yreg, nfold=3)
{
	# randomly split data into folds
	nobs <- nrow(x)
    foldid <- rep.int(1:nfold, 
    	times = ceiling(nobs/nfold))[sample.int(nobs)]
    I <- split(1:nobs, foldid)
    # create residualized objects to fill
	ytil <- dtil <- rep(NA, nobs)
	# run OOS orthogonalizations
	cat("fold: ")
	for(b in 1:length(I)){
		dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
		yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
		dhat <- predict(dfit, x[I[[b]],], type="response")
		yhat <- predict(yfit, x[I[[b]],], type="response")
		dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
		ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
		cat(b," ")
	}
	rfit <- lm(ytil ~ dtil)
	gam <- coef(rfit)[2]
	se <- sqrt(vcovHC(rfit)[2,2])
	cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))

	return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}

# OrthoML and effect of abortion access on crime (results are all very different to slides...)
resids <- orthoLTE( x=x, d=d, y=y, 
				dreg=dreg, yreg=yreg, nfold=5) 
##fold: 1  2  3  4  5  
##gamma (se) = 0.159736 (0.145697)
head(resids$dtil)
##[1]  0.022013621  0.004867683  0.004114096  0.001515087 -0.018616620 -0.017057321
head(resids$ytil)
##[1]  0.04221704 -0.04087549 -0.05199163  0.02262514  0.06287230  0.11554452
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime
##    dtil 
##0.272923

#HTE
library(foreign)

descr <- read.dta("/Users/robina/Desktop/DSHRA Weekly/06-controls/oregonhie_descriptive_vars.dta")
prgm <- read.dta("/Users/robina/Desktop/DSHRA Weekly/06-controls/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("/Users/robina/Desktop/DSHRA Weekly/06-controls/oregonhie_survey12m_vars.dta")

# nicely organized, one row per person
all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)

P <- descr[,c("person_id","household_id", "numhh_list")]
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
levels(P$numhh_list) <- c("1","2","3+")

# 12 month is the survey that really matters
# need to control for household size interacted with survey return time
Y <- s12[,c("weight_12m",
	"doc_any_12m","doc_num_mod_12m",
	"er_any_12m","er_num_mod_12m",
	"hosp_any_12m","hosp_num_mod_12m")]
Y$doc_any_12m <- as.numeric(Y$doc_any_12m=="Yes")
Y$er_any_12m <- as.numeric(Y$er_any_12m=="Yes")
Y$hosp_any_12m <- as.numeric(Y$hosp_any_12m=="Yes")

# smk_ever_12m - num19_12m are sources of heterogeneity, plus descr
X <- s12[,121:147]
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]

sapply(Y,function(y) sum(is.na(y)))
##      weight_12m      doc_any_12m  doc_num_mod_12m       er_any_12m   er_num_mod_12m     hosp_any_12m hosp_num_mod_12m 
##               0              249              300              227              260              168              202 
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]

# pull out the weights and attach doc_any to P
weights <- Y[,1]
Y <- Y[,-1]

# replace some ridiculous values in survey and drop num19
X$hhsize_12m[X$hhsize_12m>10] <- 10
X$num19_12m <- NULL

# organize to make it pretty for text
P$doc_any_12m <- Y$doc_any_12m
P <- P[,c(1,2,6,5,4,3)]
names(P)[6] <- "numhh"

# data has been cleaned in the background
head(P,n=3)
dim(P)
table(P$selected)

#Average effects  
ybar <- tapply(P$doc_any_12m, P$selected, mean)
( ATE = ybar['1'] - ybar['0'] )
##         1 
##0.05746606 
nsel <- table(P[,c("selected")])
yvar <- tapply(P$doc_any_12m, P$selected, var)
( seATE = sqrt(sum(yvar/nsel)) )
##[1] 0.006428387
ATE + c(-2,2)*seATE
##[1] 0.04460929 0.07032284


lin <- glm(doc_any_12m ~ selected + numhh, data=P);
round( summary(lin)$coef["selected",],4) # 6-7% increase in prob
##  Estimate Std. Error    t value   Pr(>|t|) 
##    0.0639     0.0065     9.9006     0.0000 

# Digression - Handling Missings (I get errors here, "cannot open file Naref.R: No such file of directory" and it states i need to give a data frame. I cannot seem to get rid of these errors)
levels(X$edu_12m)
source("naref.R")
levels(naref(X$edu_12m))

xnum <- X[,sapply(X,class)%in%c("numeric","integer")]
xnum[66:70,]
colSums(is.na(xnum))
# flag missing
xnumna <- apply(is.na(xnum), 2, as.numeric)
xnumna[66:70,]
X <- naref(X) #makes NA the base group
##     smk_avg_mod_12m birthyear_12m hhinc_pctfpl_12m hhsize_12m
##[1,]               0             0                1          1
##[2,]               0             0                0          0
##[3,]               1             0                0          0
##[4,]               0             0                0          0
##[5,]               0             1                0          0 --> same as slides, so I hope the previous lines not running won't affect my results for the following parts

# impute the missing values
mzimpute <- function(v){ 
	if(mean(v==0,na.rm=TRUE) > 0.5) impt <- 0
	else impt <- mean(v, na.rm=TRUE)
	v[is.na(v)] <- impt
	return(v) }
xnum <- apply(xnum, 2,  mzimpute)
xnum[66:70,]
## results still same as slides

# replace/add the variables in new data frame 
for(v in colnames(xnum)){
	X[,v] <- xnum[,v]
	X[,paste(v,"NA", sep=".")] <- xnumna[,v] }
X[144:147,]
##    smk_ever_12m smk_curr_12m smk_avg_mod_12m smk_quit_12m female_12m birthyear_12m employ_12m employ_det_12m    employ_hrs_12m
##414           No   not at all               0         <NA>       Male          1983        Yes  Yes, employed              <NA>
##416          Yes    every day              14           No       Male          1966       <NA>           <NA> work <20 hrs/week
##417          Yes    every day               6          Yes     Female          1961        Yes  Yes, employed work <20 hrs/week
##418          Yes   not at all               0         <NA>     Female          1960        Yes  Yes, employed work 30+ hrs/week
##    hhinc_cat_12m hhinc_pctfpl_12m race_hisp_12m race_white_12m race_black_12m race_amerindian_12m race_asian_12m race_pacific_12m
##414          <NA>         77.20707           Yes             No             No                  No             No               No
##416   $5001-$7500         24.23420            No            Yes             No                  No             No               No
##417 $10001-$12500         61.44183            No            Yes             No                  No             No               No
##418          <NA>         77.20707            No            Yes             No                  No             No               No
##    race_other_qn_12m                     edu_12m live_partner_12m live_alone_12m live_parents_12m live_relatives_12m
##414               Yes                less than hs              Yes             No               No                 No
##416                No           hs diploma or GED              Yes             No               No                Yes
##417                No                        <NA>               No             No               No                 No
##418                No vocational or 2-year degree              Yes             No               No                 No
##    live_friends_12m live_other_12m hhsize_12m dt_returned smk_avg_mod_12m.NA birthyear_12m.NA hhinc_pctfpl_12m.NA hhsize_12m.NA
##414               No             No   2.987188     2009-07                  0                0                   1             1
##416               No            Yes   5.000000     2009-09                  0                0                   0             0
##417              Yes             No   3.000000     2009-09                  0                0                   0             0
##418               No             No   4.000000     2009-09                  0                0                   1             0
##--> results seem different now

xhte <- sparse.model.matrix(~., data=cbind(numhh=P$numhh, X))[,-1]
xhte[1:2,1:4]
dim(xhte)
##[1] 8614   68, definitely different...

dxhte <- P$selected*xhte
colnames(dxhte) <- paste("d",colnames(xhte), sep=".")
htedesign <- cbind(xhte,d=P$selected,dxhte)
# include the numhh controls and baseline treatment without penalty 
htefit <- gamlr(x=htedesign, y=P$doc_any_12m, free=c("numhh2","numhh3+","d"))
gam <- coef(htefit)[-(1:(ncol(xhte)+1)), ]
round(sort(gam)[1:6],4)
round(sort(gam, decreasing=TRUE)[1:6],4)
#errors because "data length is not a sub-multiple or multple of the number of rows" and more errors...

# Consumer demand estimation and heterogeneous treatment effects 
load("/Users/robina/Desktop/DSHRA Weekly/06-controls/dominicks-beer.rda")
head(wber)
wber = wber[sample(nrow(wber), 100000), ]
head(upc)
dim(upc)
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) #ln price per 12 ounces

coef( margfit <- lm(log(MOVE) ~ lp, data=wber[,]) )
#10% increase in price decreases  quantity sold by 6%
#ATE
##(Intercept)          lp 
##  1.0539260  -0.6289747 

wber$s <- factor(wber$STORE); wber$u <- factor(wber$UPC); wber$w <- factor(wber$WEEK)
xs <- sparse.model.matrix( ~ s-1, data=wber); xu <- sparse.model.matrix( ~ u-1, data=wber); xw <- sparse.model.matrix( ~ w-1, data=wber)
# parse the item description text as a bag o' words
library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
descr <- DocumentTermMatrix(descr)
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), # convert from stm to Matrix format
              dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))
descr[1:5,1:6]
descr[287,descr[287,]!=0]
controls <- cbind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)

# naive lasso
naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[,], y=log(wber$MOVE), free=1, standardize=FALSE)
print( coef(naivefit)[1:2,] )
## intercept         lp 
## 0.2446675 -1.9770886
# orthogonal ML 
resids <- orthoLTE( x=controls, d=wber$lp, y=log(wber$MOVE), dreg=dreg, yreg=yreg, nfold=5)
##fold: 1  2  3  4  5  
##gamma (se) = -3.5871 (0.0272028)

# interact items and text with price
#lpxu <- xu*wber$lp
#colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cbind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")

eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)
# fullhte
lnwberMOVE <- log(wber[['MOVE']])
fullhte <- gamlr(x=cbind(d,controls), y=lnwberMOVE, lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])

coef(fullhte)

hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)
##actually very different to slides, mine is very skewed to the left.
