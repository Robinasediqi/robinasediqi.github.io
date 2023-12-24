#KNN
library(MASS)
data(fgl)
dim(fgl)
head(fgl, n = 2)
##[1] 214  10
##     RI    Na   Mg   Al    Si    K   Ca Ba Fe type
##1  3.01 13.64 4.49 1.10 71.78 0.06 8.75  0  0 WinF
##2 -0.39 13.89 3.60 1.36 72.73 0.48 7.83  0  0 WinF

#KNN example
par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
##shows distribution of types of glass per component

x <- scale(fgl[,1:9]) # column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) # apply function sd to columns of x

library(class) #has knn function 
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)

#EXPERIMENT 1: TESTING ACCURACY BY COMPARING PREDICTED LABELS TO ACTUAL ONES FOR K=1 AND K=5 (ideally, it is equal to 1. for k=1 that is the case, which is great. for k=5 it's equal to 0.9, which is good because it is very close to 1.)
accuracy_k1 <- sum(nearest1 == fgl$type[test]) / length(test)
accuracy_k5 <- sum(nearest5 == fgl$type[test]) / length(test)
print(paste("Accuracy (k=1):", round(accuracy_k1, 2)))
print(paste("Accuracy (k=5):", round(accuracy_k5, 2)))
##[1] "Accuracy (k=1): 1"
##[1] "Accuracy (k=5): 0.9"

#EXPERIMENT 2: LOOKING AT THE EFFECT OF SCALING VS NON SCALING (only last row is different (Tabl, Con, Tabl) so overall, the difference is not huge)
nearest1_unscaled <- knn(train = fgl[, 1:9][-test, ], test = fgl[, 1:9][test, ], cl = fgl$type[-test], k = 1)
nearest5_unscaled <- knn(train = fgl[, 1:9][-test, ], test = fgl[, 1:9][test, ], cl = fgl$type[-test], k = 5)
data.frame(fgl$type[test], nearest1_unscaled, nearest5_unscaled)

# Classification Example
credit <- read.csv("/Users/robina/Desktop/DSHRA Weekly/05-classification/credit.csv", header = TRUE)
## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## adjust foreign and purose variables, create rent variable
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
##subset data
credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]
                    
head(credit)
## Default duration amount installment age  history      purpose foreign  rent
##1       0        6   1169           4  67 terrible goods/repair foreign FALSE
##2       1       48   5951           2  22     poor goods/repair foreign FALSE
##3       0       12   2096           2  49 terrible          edu foreign FALSE
##4       0       42   7882           2  45     poor goods/repair foreign FALSE
##5       1       24   4870           3  53     poor       newcar foreign FALSE
##6       0       36   9055           2  35     poor          edu foreign FALSE
dim(credit)
##[1] 1000    9 

library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)

default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)
##plots look a lot like LASSO path plot and k-fold plot from week 4

#EXPERIMENT 3: LOOKING AT FEATURE IMPORTANCE (intercept, duration, age, purposeudedcar, duration:installment, duration:rentTRUE, installment:historygood, installment:purposenewcar, installment:porposeedu, installment:foreignforeign, age:historygood, age:historyterrible, historygood:purposenewcar, historyterrible:purposegoods/repair, historyterrible:purposeedu, historyterrible:rentFALSE, historygood:rentTRUE, purposenewcar:foreignforeign and purposeusedcar:rentTRUE are bigger than 0.000, all other ones are not)
coefficients <- coef(credscore$gamlr)
print(coefficients)

sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

#EXPERIMENT 4: CALCULATING THE OOS R^2
oos_r_squared <- 1 - credscore$cvm[credscore$seg.min] / credscore$cvm[1]
print(paste("Out-of-Sample R-squared:", round(oos_r_squared, 2)))
##[1] "Out-of-Sample R-squared: 0.09" which is quite low, but that is not necessarily very telling of how 'good' the model is

#classification rule
rule <- 1/5 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 1/5 rule
##[1] 0.6059744
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 1/5 rule
##[1] 0.07744108

# Sensitivity and specificity 
sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
##TPR = [1] 0.9233333
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity
##TNR = [1] 0.3914286

#OOS ROC curve (code did not work, so I used a different method)
# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

install.packages("pROC")

dir.create("/Users/robina/Desktop/DSHRA Weekly/05-classification/figs")

##IS
library(pROC)
pred_in_sample <- predict(credscore$gamlr, credx, type = "response")
pred_in_sample <- drop(pred_in_sample)
roc_curve <- roc(response = default, predictor = pred_in_sample)
png(file = "/Users/robina/Desktop/DSHRA Weekly/05-classification/figs/ROCCurve.png", width = 600, height = 350)
par(mai = c(.9, .9, .2, .1), mfrow = c(1, 2))
plot(roc_curve, main = "In-sample ROC Curve", col = "blue")
dev.off()

##OOS I cannot figure out how to make it work:(

par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") 

# LASSO penalized multinomial regression example 
library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variables and all their interactions with refractive index (RI).
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit

##following plots look similar to the ones in the slides, but are slightly different
plot(glassfit)
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) 
plot(glassfit$glm, xvar="lambda")

B  <- coef(glassfit, select="min"); B ## extract coefficients
B <- do.call(cbind, B) 
colnames(B) <- levels(gtype) # column names dropped in previous command. This command adds them back.

#interpreting multinomial logit coefficients
DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg; #B is a matrix. Fixed Row. Vary Columns. k is Mg, a is WinNF, b is WinF. 
##[1] -0.4430787
exp(DeltaBMg);
##[1] 0.6420567
1 - exp(DeltaBMg)
##[1] 0.3579433

probfgl <- predict(glassfit, xfgl, type="response"); dim(probfgl); head(probfgl,n=2); tail(probfgl,n=2)
#gives in-sample probabilities. Note: this is nXKX1 array. Need nXK array. To convert: 
probfgl <- drop(probfgl); #use dim(probfgl) to check dim is 214 by 6
n <- nrow(xfgl)
trueclassprobs <- probfgl[cbind(1:n, gtype)]; head(trueclassprobs,n=3); tail(trueclassprobs,n=3) 
#for each obs there is one probability that corresponds to realized shard for that obs. Last command extracts those probabilities. 
#Note use of a matrix to index a matrix.
plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
	xlab="glass type", ylab="prob( true class )") 
##as before, very similar to the slides, but not exactly the same