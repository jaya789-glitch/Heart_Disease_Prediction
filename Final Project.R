### Loading Libraries

# install.packages(c("ggplot2", "caret", "Hmisc", "rpart", "tree", "GGally", "rpart.plot"))
# install.packages("rattle")

library(ggplot2)
library(Hmisc)
library(caret)
library(dplyr)
library(GGally)
library(rpart)
library(tree)
library(rpart.plot)
library(performanceEstimation)
library(rattle)
library(ROCR)
library(pROC)
library(randomForest)


### Data Preprocessing and Exploration

# data path in the working directory where the csv file is saved
getwd()
data_path <- "heart_2020_cleaned.csv"

# read the data from the above path
heart_data <- read.csv(data_path)

# Review the data structure and head
str(heart_data)
head(heart_data)

#'data.frame'     :	319795 obs. of  18 variables:
# HeartDisease    : chr  Yes, No
# BMI             : num  
# Smoking         : chr  Yes, No
# AlcoholDrinking : chr  Yes, No
# Stroke          : chr  Yes, No
# PhysicalHealth  : num  
# MentalHealth    : num  
# DiffWalking     : chr  Yes, No
# Sex             : chr  Male, Female
# AgeCategory     : chr  18-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59,
#                        60-64, 65-69, 70-74, 75-79, 80 or older
# Race            : chr  American Indian/Alaskan Native, Asian, Black, Hispanic,
#                        Other, White
# Diabetic        : chr  Yes, No
# PhysicalActivity: chr  Yes, No
# GenHealth       : chr  Excellent, Fair, Good, Poor, Very good
# SleepTime       : num  
# Asthma          : chr  Yes, No
# KidneyDisease   : chr  Yes, No
# SkinCancer      : chr  Yes, No

# Review Summary statistics of data
summary(heart_data)

# Descriptive statistics of data
describe(heart_data)

# Check missing values by row
# No missing values found
heart_data[rowSums(is.na(heart_data))>0,]
which(is.na(heart_data))

# Omit missing values
heart_data<-na.omit(heart_data)

# Check whether rows contains NA values
check_na<-function(x){
result<- which(x=='na'| x=='NA' |x=='Na' | x=='nA')
length(result)
}
apply(heart_data, 2, check_na)

# Check for duplicates
check_duplicates <- duplicated(heart_data)
sum(check_duplicates)

# 18078 Duplicate Values
# Drop duplicates
heart_data<-distinct(heart_data)
str(heart_data)
summary(heart_data)

#'data.frame'     :	301717 obs. of  18 variables:
# HeartDisease    : chr  Yes, No
# BMI             : num  
# Smoking         : chr  Yes, No
# AlcoholDrinking : chr  Yes, No
# Stroke          : chr  Yes, No
# PhysicalHealth  : num  
# MentalHealth    : num  
# DiffWalking     : chr  Yes, No
# Sex             : chr  Male, Female
# AgeCategory     : chr  18-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59,
#                        60-64, 65-69, 70-74, 75-79, 80 or older
# Race            : chr  American Indian/Alaskan Native, Asian, Black, Hispanic,
#                        Other, White
# Diabetic        : chr  Yes, No
# PhysicalActivity: chr  Yes, No
# GenHealth       : chr  Excellent, Fair, Good, Poor, Very good
# SleepTime       : num  
# Asthma          : chr  Yes, No
# KidneyDisease   : chr  Yes, No
# SkinCancer      : chr  Yes, No


## Convert AgeCategory character to numerical averages as age should be a numerical variable

heart_data$AgeCategory[heart_data$AgeCategory=='18-24'] <- 21
heart_data$AgeCategory[heart_data$AgeCategory=='25-29'] <- 27
heart_data$AgeCategory[heart_data$AgeCategory=='30-34'] <- 32
heart_data$AgeCategory[heart_data$AgeCategory=='35-39'] <- 37
heart_data$AgeCategory[heart_data$AgeCategory=='40-44'] <- 42
heart_data$AgeCategory[heart_data$AgeCategory=='45-49'] <- 47
heart_data$AgeCategory[heart_data$AgeCategory=='50-54'] <- 52
heart_data$AgeCategory[heart_data$AgeCategory=='55-59'] <- 57
heart_data$AgeCategory[heart_data$AgeCategory=='60-64'] <- 62
heart_data$AgeCategory[heart_data$AgeCategory=='65-69'] <- 67
heart_data$AgeCategory[heart_data$AgeCategory=='70-74'] <- 72
heart_data$AgeCategory[heart_data$AgeCategory=='75-79'] <- 77
heart_data$AgeCategory[heart_data$AgeCategory=='80 or older'] <-80
heart_data$AgeCategory<-as.numeric(heart_data$AgeCategory)
summary(heart_data$AgeCategory)


### Numerical Data Distribution Plots




### BMI Distribution Plots

# Box plot between HeartDisease and BMI

ggplot(heart_data, aes(x=HeartDisease, y=BMI, fill=HeartDisease))+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Boxplot of BMI Comparison Between Heart Disease/No Heart Disease")

# Mean of Yes is greater than Mean of No, meaning people with heart disease have
# more body mass index although the difference is not far great
# There are some outliers in this distribution


# Violin Plot between HeartDisease and BMI

ggplot(heart_data, aes(x = HeartDisease, y = BMI, fill = HeartDisease))+
  geom_violin()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Violin plot of BMI Between Heart Disease/No Heart Disease")


# Multi-density distribution of BMI between Heart Disease/No Heart Disease
# Normally distributed

ggplot(heart_data, aes(x=BMI, group=HeartDisease, fill=HeartDisease))+
  geom_density()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Multi-Density Distribution of BMI Between Heart Disease/No Heart Disease")


# Density Graph for BMI

ggplot(heart_data, aes(x=BMI))+
  geom_density()+
  ggtitle("Density Graph for BMI")



### PhysicalHealth Distribution Plots

# Box plot between HeartDisease and PhysicalHealth

ggplot(heart_data, aes(x=HeartDisease, y=PhysicalHealth, fill=HeartDisease))+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Boxplot of PhysicalHealth Comparison Between Heart Disease/No Heart Disease")


# Violin Plot between HeartDisease and PhysicalHealth

ggplot(heart_data, aes(x = HeartDisease, y = PhysicalHealth, fill = HeartDisease))+
  geom_violin()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Violin plot of PhysicalHealth Between Heart Disease/No Heart Disease")


# Density Graph for PhysicalHealth

ggplot(heart_data, aes(x=PhysicalHealth))+
  geom_density()+
  ggtitle("Density Graph for PhysicalHealth")


# Multi-density distribution of PhysicalHealth between Heart Disease/No Heart Disease

ggplot(heart_data, aes(x=PhysicalHealth, group=HeartDisease, fill=HeartDisease))+
  geom_density()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Multi-Density Distribution of PhysicalHealth Between Heart Disease/No Heart Disease")



### MentalHealth Distribution Plots

# Box plot between HeartDisease and MentalHealth

ggplot(heart_data, aes(x=HeartDisease, y=MentalHealth, fill=HeartDisease))+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Boxplot of MentalHealth Comparison Between Heart Disease/No Heart Disease")


# Violin Plot between HeartDisease and MentalHealth

ggplot(heart_data, aes(x = HeartDisease, y = MentalHealth, fill = HeartDisease))+
  geom_violin()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Violin plot of MentalHealth Between Heart Disease/No Heart Disease")


# Density Graph for MentalHealth

ggplot(heart_data, aes(x=MentalHealth))+
  geom_density()+
  ggtitle("Density Graph for MentalHealth")


# Multi-density distribution of MentalHealth between Heart Disease/No Heart Disease

ggplot(heart_data, aes(x=MentalHealth, group=HeartDisease, fill=HeartDisease))+
  geom_density()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Multi-Density Distribution of MentalHealth Between Heart Disease/No Heart Disease")



### SleepTime Distribution Plots

# Box plot between HeartDisease and SleepTime

ggplot(heart_data, aes(x=HeartDisease, y=SleepTime, fill=HeartDisease))+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Boxplot of SleepTime Comparison Between Heart Disease/No Heart Disease")


# Violin Plot between HeartDisease and SleepTime

ggplot(heart_data, aes(x = HeartDisease, y = SleepTime, fill = HeartDisease))+
  geom_violin()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Violin plot of SleepTime Between Heart Disease/No Heart Disease")


# Density Graph for SleepTime

ggplot(heart_data, aes(x=SleepTime))+
  geom_density()+
  ggtitle("Density Graph for SleepTime")


# Multi-density distribution of SleepTime between Heart Disease/No Heart Disease

ggplot(heart_data, aes(x=SleepTime, group=HeartDisease, fill=HeartDisease))+
  geom_density()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("Red", "Green"))+
  ggtitle("Multi-Density Distribution of SleepTime Between Heart Disease/No Heart Disease")



### Categorical Variables Distribution

# Smoking Stacked Bar Plot

ggplot(heart_data, aes(x=Smoking, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in People with Smoking Habits")+
  ylab('Percentage')

# AlcoholDrinking Stacked Bar Plot

ggplot(heart_data, aes(x=AlcoholDrinking, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in People with AlcoholDrinking Habits")+
  ylab('Percentage')

# Stroke Stacked Bar Plot

ggplot(heart_data, aes(x=Stroke, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in People with Stroke History")+
  ylab('Percentage')

# DiffWalking Stacked Bar Plot

ggplot(heart_data, aes(x=DiffWalking, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in People with Difficulty in Walking")+
  ylab('Percentage')

# Gender Stacked Bar Plot

ggplot(heart_data, aes(x=Sex, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in different genders")+
  ylab('Percentage')

# AgeCategory Stacked Bar Plot

ggplot(heart_data, aes(x=AgeCategory, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease across different age categories")+
  ylab('Percentage')

# Race Stacked Bar Plot

ggplot(heart_data, aes(x=Race, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease across different races")+
  ylab('Percentage')

# Diabetic Stacked Bar Plot

ggplot(heart_data, aes(x=Diabetic, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in People with Diabetes")+
  ylab('Percentage')

# PhysicalActivity Stacked Bar Plot

ggplot(heart_data, aes(x=PhysicalActivity, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in people with Physical Activity")+
  ylab('Percentage')

# GenHealth Stacked Bar Plot

ggplot(heart_data, aes(x=GenHealth, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in people with different Health Conditions")+
  ylab('Percentage')

# Asthma Stacked Bar Plot

ggplot(heart_data, aes(x=Asthma, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in people with Asthma")+
  ylab('Percentage')

# KidneyDisease Stacked Bar Plot

ggplot(heart_data, aes(x=KidneyDisease, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in people with Kidney Disease")+
  ylab('Percentage')

# SkinCancer Stacked Bar Plot

ggplot(heart_data, aes(x=SkinCancer, fill=HeartDisease))+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("red", "green"))+
  geom_bar(position="fill")+
  ggtitle("Percentage of Heart Disease in people with SkinCancer")+
  ylab('Percentage')



## Dummy Encoding

dummy_data <- data.frame(heart_data)

## Two Categorical Variables

dummy_data$SmokingYes <- ifelse(dummy_data$Smoking=='Yes', 1 ,0)
dummy_data$AlcoholDrinkingYes <- ifelse(dummy_data$AlcoholDrinking=='Yes',1,0)
dummy_data$StrokeYes <- ifelse(dummy_data$Stroke=='Yes',1,0)
dummy_data$DiffWalkingYes <- ifelse(dummy_data$DiffWalking=='Yes',1,0)
dummy_data$SexMale <- ifelse(dummy_data$Sex=='Male',1,0)
dummy_data$PhysicalActivityYes <- ifelse(dummy_data$PhysicalActivity=='Yes',1,0)
dummy_data$AsthmaYes <- ifelse(dummy_data$Asthma=='Yes',1,0)
dummy_data$KidneyDiseaseYes <- ifelse(dummy_data$KidneyDisease=='Yes',1,0)
dummy_data$SkinCancerYes <- ifelse(dummy_data$SkinCancer=='Yes',1,0)

## Multi-Categorical Variables

# Race Column with 6 distinct categories will have 5 dummy variables with "White" as baseline

dummy_data$RaceAI_AN <- ifelse(dummy_data$Race=='American Indian/Alaskan Native', 1 ,0)
dummy_data$RaceAsian <- ifelse(dummy_data$Race=='Asian', 1 ,0)
dummy_data$RaceBlack <- ifelse(dummy_data$Race=='Black', 1 ,0)
dummy_data$RaceHispanic <- ifelse(dummy_data$Race=='Hispanic', 1 ,0)
dummy_data$RaceOther <- ifelse(dummy_data$Race=='Other', 1 ,0)

# Diabetic Column with 4 distinct categories will have 3 dummy variables with "No" as baseline

dummy_data$DiabeticYes <- ifelse(dummy_data$Diabetic=='Yes', 1 ,0)
dummy_data$DiabeticDurPreg <- ifelse(dummy_data$Diabetic=='Yes (during pregnancy)', 1 ,0)
dummy_data$DiabeticBorderline <- ifelse(dummy_data$Diabetic=='No, borderline diabetes', 1 ,0)

# GenHealth Column with 5 distinct categories will have 4 dummy variables with "Very good" as baseline

dummy_data$GenHealthExcellent <- ifelse(dummy_data$GenHealth=='Excellent', 1 ,0)
dummy_data$GenHealthFair <- ifelse(dummy_data$GenHealth=='Fair', 1 ,0)
dummy_data$GenHealthPoor <- ifelse(dummy_data$GenHealth=='Poor', 1 ,0)
dummy_data$GenHealthGood <- ifelse(dummy_data$GenHealth=='Good', 1 ,0)


# Drop Categorical Variables in dummy data after dummy encoding

final_data <- subset(dummy_data, select = -c(Smoking, AlcoholDrinking, Stroke, DiffWalking, Sex,
                                             Race, Diabetic, PhysicalActivity, GenHealth, Asthma, KidneyDisease, SkinCancer))

final_data$HeartDisease <- as.factor(final_data$HeartDisease)
# final_data$HeartDisease <- ifelse(final_data$HeartDisease=='Yes', 1 ,0)


## Impute Outliers

outlier_impute <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}

summary(final_data$BMI)
final_data$BMI <- outlier_impute(final_data$BMI)
summary(final_data$BMI)

summary(final_data$PhysicalHealth)
final_data$PhysicalHealth <- outlier_impute(final_data$PhysicalHealth)
summary(final_data$PhysicalHealth)

summary(final_data$MentalHealth)
final_data$MentalHealth <- outlier_impute(final_data$MentalHealth)
summary(final_data$MentalHealth)

summary(final_data$AgeCategory)
final_data$AgeCategory <- outlier_impute(final_data$AgeCategory)
summary(final_data$AgeCategory)


## Normalize the numerical features using min and max

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

final_data[c(2,3,4,5,6)] <- lapply(final_data[c(2,3,4,5,6)],normalize)
summary(final_data[c(2,3,4,5,6)])

## Additional Descriptive Statistics for the final data
stats <- function(x) {
  return(data.frame(mean(x), median(x), sd(x), var(x), length(x)))}

stats_list <- lapply(final_data[2:27], stats)
stats_list


### Modelling


# Split Data into Train 75% and Test 25%

set.seed(3333)

train.index <- createDataPartition(final_data$HeartDisease, p = .7, list = FALSE)
train_data <- final_data[ train.index,]
test_data <- final_data[-train.index,]

# Addressing Class Imbalancy

table(train_data$HeartDisease)
train <- smote(HeartDisease ~ ., data  =train_data)
table(train$HeartDisease)

table(test_data$HeartDisease)
test <- smote(HeartDisease ~ ., data  =test_data)
table(test$HeartDisease)


## Decision Tree

ctrl <- trainControl(method="repeatedcv", number = 10, repeats = 5)

set.seed(3333)

# Train the decision tree model
dtree_train <- train(HeartDisease ~ ., data = train,
                     method = "rpart",
                     parms = list(split = "information"),
                     trControl=ctrl,
                     tuneLength = 10)

# Decision tree model summary
print(dtree_train)

# Decision Tree Plots
fancyRpartPlot(dtree_train$finalModel)
prp(dtree_train$finalModel, box.palette = "Reds", tweak = 1.2)
plot(dtree_train, col="red")
plot(varImp(dtree_train))

# Confusion Matrix for the trained model
confusionMatrix(dtree_train,
  positive = 'Yes',
  mode = "everything")

# Prediction using decision tree model
dtree_pred <- predict(dtree_train, newdata = test, type="raw")

# Confusion Matrix for the decision tree predictions made
confusionMatrix(dtree_pred, test$HeartDisease, 
                mode = "everything", positive = 'Yes')

# ROC Curve
dtree_pred <- ifelse(dtree_pred == 'Yes', 1, 0)
table(test$HeartDisease, dtree_pred==1)
ROCRpred <- prediction(dtree_pred, test$HeartDisease)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, main = "ROC Curve for Decision Tree Model")

# Area Under Curve
auc(test$HeartDisease, dtree_pred)



## Logistic Regression Model

glm_ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)

set.seed(3333)

# Train the glm model
lm.train <- train(HeartDisease ~ ., data=train, 
                  method="glm",
                  trControl=glm_ctrl,
                  tuneLength = 10)

# Model Summary
lm.train

# Predict using glm model
lm.train.predict = predict(lm.train, test, type="raw")

# Confusion Matrix for the predicted glm model
confusionMatrix(lm.train.predict, test$HeartDisease, 
                mode = "everything", positive = 'Yes')

# Variable Importance Plot
plot(varImp(lm.train))


# ROC Curve
lm.train.predict <- ifelse(lm.train.predict == 'Yes', 1, 0)
table(test$HeartDisease, lm.train.predict==1)
ROCRpred <- prediction(lm.train.predict, test$HeartDisease)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, main= "ROC Curve for Logistic Regression Model")

# Area Under Curve
auc(test$HeartDisease, lm.train.predict)


## Random Forest Model

# Train the random forest model
rf<-randomForest(HeartDisease~.,ntree=500,data=train)
rf

# Random Forest Plots
plot(rf$err.rate[,1], type="l", main="Random Forest Error Rate", xlab="Number of Trees")
varImpPlot(rf, main="Variable Importance Plot for Random Forest")

# Prediction using Random Forest model
rfpred<-predict(rf, test, type="response")

rft<-table(test$HeartDisease,rfpred)
rft

accuracy <- sum(diag(rft))/nrow(test)
accuracy


# Confusion Matrix
confusionMatrix(rfpred,test$HeartDisease)


# ROC Curve
rfpred <- ifelse(rfpred == 'Yes', 1, 0)
table(test$HeartDisease, rfpred==1)
ROCRpred <- prediction(rfpred, test$HeartDisease)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, main= "ROC Curve for Random Forest model")

# Area Under Curve
auc(test$HeartDisease, rfpred)
