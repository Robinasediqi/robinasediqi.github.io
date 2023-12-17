#importing data
CEO_Diary <- read.csv("/Users/robina/Desktop/DSHRA Weekly/01-introduction/survey_response_data (2).csv", header = TRUE)

#viewing time diaries for CEOs
View(CEO_Diary)

#viewing a subset of the data
CEO_Diary[1:15,c(1:5,37, 39, 40)] 

##EXPERIMENT: picking different datapoints for the subset
#1: looking at the first 10 obs
CEO_Diary[1:10,c(1:5,37, 39, 40)] 
#2: looking at observations 50-75
CEO_Diary[50:75,c(1:5,37, 39, 40)] 
#3: looking at the first 6 columns and 37-40
CEO_Diary[1:15,c(1:6,37:40)] 

#finding out the class of a variable (all characters)
apply(CEO_Diary,2,class)

#finding out how many rows are in the dataset (225721)
nrow(CEO_Diary)

#summary statistics of first five columns)
summary(CEO_Diary[1:5])

#checking working directory
getwd()

#creating fig directory
dir.create("/Users/robina/Desktop/DSHRA Weekly/01-introduction/figs")

#graphical analysis (creates barplot of a column of data)
png(file = "/Users/robina/Desktop/DSHRA Weekly/01-introduction/figs/CEOTypes.png", width = 800, height = 400)
  par(mar=c(9, 3 ,1,1))
  barplot(prop.table(table(CEO_Diary$type)), las=2)
  dev.off()

#function of "barplot(prop.table(table(CEO_Diary$type)), las=2)" (makes refquency table, proportion table)
barplot(prop.table(table(CEO_Diary$type)));
table(CEO_Diary$type);
prop.table(table(CEO_Diary$type));

#regression
fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)

##EXPERIMENTS: different variables
#1: dependent variable: finance (98414 obs. deleted due to missingness)
fit <- glm(finance ~ consultants + politicians, data=CEO_Diary); summary(fit)
#2: additional independent variable: unions (again, 98414 obs. deleted due to missingness)
fit <- glm(strategy ~ consultants + politicians + unions, data=CEO_Diary); summary(fit)