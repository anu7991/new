#installing and loading required packages
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("DataExplorer")
library(DataExplorer)
library(caret)
install.packages("caTools")
library(caTools)
#loading required file
db = read_csv("C:/Users/gadda/Downloads/ml-latest-small/ml-latest-small/diabetes.csv")
db <- db %>%
  mutate(Insulin = replace(Insulin, Insulin == "0", NA))
is.na(db$Insulin)
db$Insulin
#replacing NA values with Median of its observations
db = db %>% mutate(Insulin = replace(Insulin,is.na(Insulin),median(Insulin ,na.rm=T)))

db = db %>% mutate(BloodPressure = replace(BloodPressure, BloodPressure == "0" , NA))
db = db %>% mutate(BloodPressure = replace(BloodPressure,is.na(BloodPressure),median(BloodPressure ,na.rm=T)))

db = db %>% mutate(SkinThickness = replace(SkinThickness,SkinThickness == "0",NA))
db = db %>% mutate(SkinThickness = replace(SkinThickness,is.na(SkinThickness),median(SkinThickness, na.rm=T)))
glimpse(db)
#checking the distributions
ggplot(db,aes(x = SkinThickness)) + geom_histogram(binwidth = 0.25)
db %>% count(SkinThickness)
db = db %>% mutate(Glucose = replace(Glucose,Glucose == "0",NA))
db = db %>% mutate(Glucose = replace(Glucose,is.na(Glucose),median(Glucose, na.rm=T)))
glimpse(db)
ggplot(db,aes(x= Glucose)) +geom_histogram(binwidth = 0.25)
ggplot(db,aes(x= Outcome ,y = Glucose)) + geom_point()
db$Outcome=as.factor(db$Outcome)
plot_correlation(db,type = 'continous','Review.Date')
create_report(db)
#implementing feature engineering 
#implementing a model with 4 highlyly correlated variables on outcome
db1= db %>% select(Glucose,BMI,Age,Pregnancies,Outcome) 
str(db1)
set.seed(100)
traindataindex = createDataPartition(db1$Outcome,p=0.8,list=F)
train1=db1[traindataindex, ]
test1=db1[-traindataindex, ]
r = glm(Outcome ~ Glucose + BMI + Age + Pregnancies,data = train1,family = 'binomial')
summary(r)
#predicting outcome
pred = predict(r,newdata = test1,type = 'response')
pred
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train1$Outcome
mean(y_pred==y_act)
#implementing a model with all features included in dataset
str(db)
set.seed(100)
sample          = sample.split(db$Outcome, SplitRatio = 0.75)
trainingData    = subset(db, sample == TRUE)
testData        = subset(db, sample == FALSE)

logmod = glm(Outcome ~ Pregnancies +Glucose+BloodPressure+SkinThickness+BMI+Age+DiabetesPedigreeFunction+Insulin,data = trainingData,family = binomial)
summary(logmod)
pred1=predict(logmod,newdata = testData,type = 'response')
pred1
x_pred_num=ifelse(pred1 > 0.5,1,0)
x_pred=factor(x_pred_num,levels = c(0,1))
x_act =trainingData$Outcome
mean(x_pred==x_act)



