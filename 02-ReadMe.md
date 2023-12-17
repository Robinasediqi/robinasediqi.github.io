#open data
data <- read.csv("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/web-browsers.csv", header = TRUE)

browser <- read.csv("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/web-browsers.csv")
  dim(browser)
  head(browser)
  
mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)
##[1] 1946.439
##[1] 6461.925
##[1] 80.3861

  B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
##[1] 80.97877
  
  B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
    
     h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
#can you explain why we need each term in the last expression? 
  lines(xfit, yfit, col = "black", lwd = 2)
##see graph
  
##EXPERIMENT: what happens when we do not replace?
  B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=FALSE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
##[1] 0
##sd becomes 0 because there is no variation

##EXPERIMENT: what happens when B<-100?
B <- 100
  mub <- c()
  for (b in 1:100){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
##[1] 74.12131
##sd went down by about 6

#bootstrapping regressions
  B <- 1000
  betas <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
    betas <- rbind(betas, coef(reg_b))
  }; head(betas, n=3)
##        intercept broadband anychildren
##[1,]    5.664813 0.5726946  0.08489925
##[2,]    5.638818 0.5832410  0.11113138
##[3,]    5.663537 0.5328437  0.11127914  

#estimating covariance between estimaters
cov(betas[,"broadband"], betas[,"anychildren"])
##[1] -9.728007e-05

#Benjamini-Hochberg (BH) algorithm example 1
dir.create("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/figs")
  browser <- read.csv("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/web-browsers.csv")
  spendy <- glm(log(spend) ~ . -id, data=browser)
  round(summary(spendy)$coef,2)
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
  pvalrank <- rank(pval)
  reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
  png(file="/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/figs/BHAlgoExample.png",
  width=600, height=350)
  plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
  lines(pvalrank, (0.1/9)*pvalrank)
  dev.off()
  
#Benjamini-Hochberg (BH) algorithm example 2
##check dimensions (1477  201)
SC <- read.csv("/Users/robina/Desktop/DSHRA Weekly/02-uncertainty/semiconductor.csv", header = TRUE)
dim(SC)
##Regression (-1 drops the intercept) (warning message: glm.fit: fitted probabilities numerically 0 or 1 occured)
full <- glm(FAIL ~ ., data=SC, family=binomial)
pvals <- summary(full)$coef[-1,4]
##histogram
hist(pvals, xlab="p-value", main="", col="lightblue")
##False discovery rate ([1] 0.01217043)
fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
   ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  return(alpha)
}
fdr_cut(pvals)