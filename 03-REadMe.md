#importing data
oj <- read.csv("/Users/robina/Desktop/DSHRA Weekly/03-regression/oj.csv", header = TRUE)

#running regression, base group is house brand orange juice
head(oj, n=5)    
tail(oj, n=5)    
glm(log(sales) ~ brand + log(price), data=oj)
##    (Intercept)  brandminute.maid    brandtropicana        log(price)  
         10.8288            0.8702            1.5299           -3.1387  
##Degrees of Freedom: 28946 Total (i.e. Null);  28943 Residual
##Null Deviance:	    30080 
##Residual Deviance: 18220 	AIC: 68760

#dummies are created in background via
x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)

#changing base group (now tropicana is base group)
oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)
oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

#regression
glm(log(sales) ~ log(price)*brand*feat, data=oj)
##                     (Intercept)                        log(price)                  brandminute.maid  
##                         10.4066                           -2.7742                            0.0472  
##                  brandtropicana                              feat       log(price):brandminute.maid  
##                          0.7079                            1.0944                            0.7829  
##       log(price):brandtropicana                   log(price):feat             brandminute.maid:feat  
##                          0.7358                           -0.4706                            1.1729  
##             brandtropicana:feat  log(price):brandminute.maid:feat    log(price):brandtropicana:feat  
##                          0.7853                           -1.1092                           -0.9861  
##Degrees of Freedom: 28946 Total (i.e. Null);  28935 Residual
##Null Deviance:	    30080 
##Residual Deviance: 13970 	AIC: 61090

#EXPERIMENT 1: NO LOG
glm(sales ~ price*brand*feat, data=oj)
##                (Intercept)                        price             brandminute.maid               brandtropicana  
##                    39011.3                     -15192.4                     -11894.1                        477.9  
##                       feat       price:brandminute.maid         price:brandtropicana                   price:feat  
##                   128543.1                       7143.7                       5263.5                     -64317.0  
##      brandminute.maid:feat          brandtropicana:feat  price:brandminute.maid:feat    price:brandtropicana:feat  
##                    40838.4                     -42015.3                      -3655.2                      35359.6  
##Degrees of Freedom: 28946 Total (i.e. Null);  28935 Residual
##Null Deviance:	    2.185e+13 
##Residual Deviance: 1.249e+13 	AIC: 657700

#EXPERIMENT 2: REGRESSING LOG SALES ON LOG PRICE, NO BRAND 
glm(log(sales) ~ log(price), data=oj)
##(Intercept)   log(price)  
##     10.423       -1.601  
##Degrees of Freedom: 28946 Total (i.e. Null);  28945 Residual
##Null Deviance:	    30080 
##Residual Deviance: 23820 	AIC: 76510

#EXPERIMENT 2.2:REGRESSING LOG SALES ON BRAND, LOG PRICE AS CONTROL (forgot that's the same thing as before, just different order so the coefficients are the same)
glm(log(sales) ~ brand + log(price), data=oj)
##     (Intercept)  brandminute.maid    brandtropicana        log(price)  
##         10.8288            0.8702            1.5299           -3.1387  
##Degrees of Freedom: 28946 Total (i.e. Null);  28943 Residual
##Null Deviance:	    30080 
##Residual Deviance: 18220 	AIC: 68760

#EXPERIMENT 3: CHANGING REFERENCE GROUP TO MINUTE MAID (now minute maid is the intercept, and dominicks and tropicana are the dummies)
oj$mybrand = relevel(oj$brand, "minute.maid")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)
##1           1                0                1   1.353255
##2           1                0                1   1.353255
##3           1                0                1   1.353255
##4           1                0                1   1.353255
##5           1                0                1   1.353255
##6           1                0                1   1.353255

#logistic regression
email <- read.csv("/Users/robina/Desktop/DSHRA Weekly/03-regression/spam.csv", header = TRUE)
dim(email)
colnames(email)  

glm(spam ~ ., data=email, family='binomial')
##               (Intercept)                   word_make                word_address                    word_all  
##                -1.9682470                  -0.5529572                  -0.1338696                  -0.4946420  
##                   word_3d                    word_our                   word_over                 word_remove  
##                 0.8301668                   1.1252843                   0.2750951                   2.4881234  
##             word_internet                  word_order                   word_mail                word_receive  
##                 0.9333968                   0.2196402                   0.3081450                  -0.4391531  
##                 word_will                 word_people                 word_report              word_addresses  
##                -0.3105941                  -0.9585867                   0.8466587                   1.2553331  
##                 word_free               word_business                  word_email                    word_you  
##                 1.5427059                   1.0566330                  -0.5200992                   0.1628054  
##               word_credit                   word_your                   word_font                    word_000  
##                 0.4160407                   0.6949764                   1.2999227                   1.0349534  
##                word_money                     word_hp                    word_hpl                 word_george  
##                 1.7300810                  -3.6043950                  -0.1806108                  -5.7798414  
##                  word_650                    word_lab                   word_labs                 word_telnet  
##                 2.1047235                  -0.6635103                  -0.1804292                  -2.3018083  
##                  word_857                   word_data                    word_415                     word_85  
##                -1.4454869                  -0.7849383                   0.9195251                  -1.7312335  
##           word_technology                   word_1999                  word_parts                     word_pm  
##                 0.3719642                  -1.0922412                   1.5572057                  -0.5686254  
##               word_direct                     word_cs                word_meeting               word_original  
##                -0.2828228                  -6.2966891                  -2.5034078                  -1.2428824  
##              word_project                     word_re                    word_edu                  word_table  
##                -1.6185685                  -1.0257826                  -2.4268251                   0.2853855  
##           word_conference              char_semicolon               char_leftbrac         char_leftsquarebrac  
##                -2.2234335                  -0.2928835                   0.1256978                  -0.3345468  
##              char_exclaim                 char_dollar                  char_pound  capital_run_length_average  
##                 1.3427284                   1.8707164                  -0.8186884                  -0.0031379  
##capital_run_length_longest    capital_run_length_total  
##                 0.0060448                   0.0008344  
##Degrees of Freedom: 4600 Total (i.e. Null);  4543 Residual
##Null Deviance:	    6170 
##Residual Deviance: 1549 	AIC: 1665

#logit example
predict(spammy, newdata = email[c(1,4000),], type="response")
##        1      4000 
##0.8839073 0.1509989 

#deviance and likelihood
summary(spammy)$deviance
##[1] 1548.66
summary(spammy)$null.deviance
##[1] 6170.153

#R^2 (0.749)
D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2
##[1] 0.7490079
