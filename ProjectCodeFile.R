##############################################
## READ IN Libraries
##############################################

library(dplyr)
library(caret)
library(class)
library(e1071)
library(reshape2)
library(ggplot2)
library(GGally)
library(tidyverse)


##############################################
## Create Function to Process data sets.
##############################################

processData <- function(df){

  ##### Add new Columns  #####
  
  df$TravelScore = 0
  df$DistanceScore = 0
  df$OvertimeScore = 0
  df$TotalWLBScore = 0
  
  df$PayScore = 0
  df$RaiseScore = 0
  df$StockScore = 0 
  df$TotalPayScore = 0
  
  df$TrainingScore = 0
  df$PromotionScore = 0
  df$RoleScore = 0
  df$TotalGrowthScore = 0
  
  df$MgrScore1 = 0
  df$MgrScore2 = 0
  df$MgrScore3 = 0
  df$MgrScore4 = 0
  df$TotalMgrScore = 0
  df$TotalEnvScore = 0
  
  ##### Process Data  #####
  
  for(i in 1 : length(df$ID))
  {
    ## All Scores are the higher the number the more likely to stay.  ##
    #####  Work Life Balance Score  #####
    
    df$TravelScore[i] = 
      if(df$BusinessTravel[i] == "Non-Travel"){2} else if (df$BusinessTravel[i] == "Travel_Rarely"){1} else {0}
    
    df$DistanceScore[i] = ((floor(df$DistanceFromHome[i]/10)-2) *-1)  # the further from home the more likely to leave.
    
    df$TotalWLBScore[i] = df$TravelScore[i] + df$DistanceScore[i] + df$WorkLifeBalance[i]
    
    
    #####  Pay Score  #####
    
    # Job level taken into account with the thought that pay rate at a level 1 has a lot of influence for someone 
    # staying or leaving, but has much less influence for a level 5 employee.
    df$PayScore[i] = round((df$MonthlyIncome[i] / df$JobLevel[i]),-2)
    
    df$RaiseScore[i] = (df$PercentSalaryHike[i] * 100)  # a weight adjustment to keep in line with the Pay Score.
    
    df$StockScore[i] = (df$StockOptionLevel[i] * 1000) # a weight adjustment to keep in line with the Pay Score.
    
    df$TotalPayScore[i] = df$PayScore[i] + df$RaiseScore[i] + df$StockScore[i]
    
    
    #####  Growth Score #####
    
    # The logic for the training score was a person just starting would expect more training than a person who was a level 5.  
    # And I adjusted the weight of this column to make it more revelavent to the growth score.
    df$TrainingScore[i] = (floor((df$TrainingTimesLastYear[i] * (1/df$JobLevel[i]))) * 10)
    
    # 60 was used as a reasonable max amount of time an employee might work for a company.
    # Job level was taken into account with the thought that a level 5 employee does not have a large expectation to be promoted 
    # but a level 1 employee has a large expection to be promoted.
    df$PromotionScore[i] = ceiling((60 - if(df$YearsSinceLastPromotion[i] == 0){df$YearsAtCompany[i]} else {df$YearsSinceLastPromotion[i]}) * (1/df$JobLevel[i]))
      
    df$RoleScore[i] = ceiling((60 - if(df$YearsInCurrentRole[i] == 0){df$YearsAtCompany[i]} else {df$YearsInCurrentRole[i]}) * (1/df$JobLevel[i]))
      
    df$TotalGrowthScore[i] = df$TrainingScore[i] + df$PromotionScore[i] + df$RoleScore[i]
    
    
    #####  Environmental Score  #####
    
    df$MgrScore1[i] = 
      if(df$RelationshipSatisfaction[i] == 1) ## Very Un-happy with Manager so the longer the person stays the more likely they are to leave.
      {  
        if(df$YearsWithCurrManager[i] >= 0 && df$YearsWithCurrManager[i] < 3){
          0
        } else if(df$YearsWithCurrManager[i] >= 3 && df$YearsWithCurrManager[i] < 6){
          -1
        } else if(df$YearsWithCurrManager[i] >= 6 && df$YearsWithCurrManager[i] < 9){
          -2
        } else if(df$YearsWithCurrManager[i] >= 9 && df$YearsWithCurrManager[i] < 12){
          -3
        } else if(df$YearsWithCurrManager[i] >= 12){
          -4
        }
      } else {0}

    df$MgrScore2[i] =
      if(df$RelationshipSatisfaction[i] == 2)  # Somewhat un-happy with manager so the longer they have the same manager the more likely to leave.
      {
        if(df$YearsWithCurrManager[i] >= 0 && df$YearsWithCurrManager[i] < 3)
        {
          1
        } else if(df$YearsWithCurrManager[i] >= 3 && df$YearsWithCurrManager[i] < 6)
        {
          0
        } else if(df$YearsWithCurrManager[i] >= 6 && df$YearsWithCurrManager[i] < 9)
        {
          -1
        } else if(df$YearsWithCurrManager[i] >= 9 && df$YearsWithCurrManager[i] < 12)
        {
          -2
        } else if(df$YearsWithCurrManager[i] >= 12)
        {
          -3
        }
      } else {0}

    df$MgrScore3[i] =
      if(df$RelationshipSatisfaction[i] == 3)  # Happy with manager so the longer they have the same manager the more likely to stay.
      {
        if(df$YearsWithCurrManager[i] >= 0 && df$YearsWithCurrManager[i] < 3)
        {
          1
        } else if(df$YearsWithCurrManager[i] >= 3 && df$YearsWithCurrManager[i] < 6)
        {
          2
        } else if(df$YearsWithCurrManager[i] >= 6 && df$YearsWithCurrManager[i] < 9)
        {
          3
        } else if(df$YearsWithCurrManager[i] >= 9 && df$YearsWithCurrManager[i] < 12)
        {
          4
        } else if(df$YearsWithCurrManager[i] >= 12)
        {
          4
        }
      } else {0}

    df$MgrScore4[i] =
      if(df$RelationshipSatisfaction[i] == 4) # Very Happy with manager so the longer they have the same manager the more likely to stay.
      {
        if(df$YearsWithCurrManager[i] >= 0 && df$YearsWithCurrManager[i] < 3)
        {
          3
        } else if(df$YearsWithCurrManager[i] >= 3 && df$YearsWithCurrManager[i] < 6)
        {
          4
        } else if(df$YearsWithCurrManager[i] >= 6 && df$YearsWithCurrManager[i] < 9)
        {
          4
        } else if(df$YearsWithCurrManager[i] >= 9 && df$YearsWithCurrManager[i] < 12)
        {
          4
        } else if(df$YearsWithCurrManager[i] >= 12)
        {
          4
        }
      } else {0}

    df$TotalMgrScore[i] = df$MgrScore1[i] + df$MgrScore2[i] + df$MgrScore3[i] + df$MgrScore4[i]

    # Adding the columns I believe identify the enviromental factors for an enployee leaving or staying.
    df$TotalEnvScore[i] = df$TotalMgrScore[i] + df$EnvironmentSatisfaction[i] + df$JobInvolvement[i] + df$JobSatisfaction[i]

  }
  
  df  # Returns the dataframe
}

##############################################
## READ IN Data
##############################################

attData <- processData(read.csv("CaseStudy2-data.csv"))      #StringsAsFactors = TRUE

##############################################
## Heatmap Correlation Matrix
##############################################

corrData <- select(attData, Age,JobLevel, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, PerformanceRating,
                   TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)

corrData$Attrition = as.integer(attData$Attrition)

cm <- round(cor(corrData),2)

hmData <- melt(cm)


hm <- ggplot(hmData, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()


hm + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(-5, 0),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



##############################################
## EDA for Work Life Balance variables
##############################################
# The work Life Balance factors do not seem to have much of an impact to determine attrition.
attData %>% select(Attrition, BusinessTravel, DistanceFromHome, WorkLifeBalance) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")
  
attData %>% select(Attrition, BusinessTravel) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Business Travel vs Attrition")

attData %>% select(Attrition, DistanceFromHome) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Distance From Home vs Attrition")

attData %>% select(Attrition, WorkLifeBalance) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance vs Attrition")

# My score for Work Life Balance seems normally distributed and might be better to determine attrition.
attData %>% select(Attrition, TotalWLBScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance Score vs Attrition")


##############################################
## EDA for Compensation variables
##############################################
# Monthly Income seems to be a good indicatior for determining attrition.
attData %>% select(Attrition, MonthlyIncome, PercentSalaryHike, StockOptionLevel) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Compensation factors vs Attrition")

# The Total Pay Score seems normally distributed and a good indicator of determining attrition.
attData %>% select(Attrition, TotalPayScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Compensation Score vs Attrition")


##############################################
## EDA Career Growth variables
##############################################
# Job Level seems to be a good indicator for determining attrition, but the other variables not so much.
attData %>% select(Attrition, JobLevel, TrainingTimesLastYear, YearsSinceLastPromotion, YearsInCurrentRole) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Career Growth factors vs Attrition")
  
# Total Growth score seems like a good indicator of attrition, but is not normally distributed.
attData %>% select(Attrition, TotalGrowthScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Total Growth Score vs Attrition")


##############################################
## EDA Environmental variables
##############################################
# The individual variables do not look very good at determining attrition
attData %>% select(Attrition, RelationshipSatisfaction, YearsWithCurrManager, JobInvolvement, JobSatisfaction) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Environmental factors vs Attrition")

# Does not look usefull on it's own.
attData %>% select(Attrition, RelationshipSatisfaction) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Manager Satisfaction vs Attrition")

# This might be usefull but is left skewed.
attData %>% select(Attrition, YearsAtCompany) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Years at Company vs Attrition")

# Again might be usefull but is left skewed and really needs to be combined 
# with RelationshipSatisfaction to be of use.
attData %>% select(Attrition, YearsWithCurrManager) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Years with Current Manager vs Attrition")

# The Total Environmental Score does look normally distributed and might be a good indicator of attrition.
attData %>% select(Attrition, TotalEnvScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Environmental Score vs Attrition")

  
##############################################
## EDA Other variables
##############################################
# Might be usefull, is a little left skewed.
attData %>% select(Attrition, Age) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Age vs Attrition")

# Might be usefull, Divorced people don't leave?
attData %>% select(Attrition, MaritalStatus) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Marital Status vs Attrition")



# Does not look usefull.
attData %>% select(Attrition, Education) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Education vs Attrition")

# Does not look usefull.
attData %>% select(Attrition, Gender) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Gender vs Attrition")

# Does not look to be usefull.
attData %>% select(Attrition, OverTime) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Overtime vs Attrition")



##############################################
## Select Data for Models.
##############################################  

ds_a <- attData %>%
  select(ID,  # 1
         Age, MaritalStatus, Education, JobInvolvement,   # 2, 3, 4, 5
         DistanceFromHome, WorkLifeBalance, TravelScore, # 6, 7, 8
         MonthlyIncome, PercentSalaryHike, StockOptionLevel, JobLevel, # 9, 10, 11, 12
         TrainingTimesLastYear, YearsInCurrentRole, YearsSinceLastPromotion,  # 13, 14, 15
         YearsWithCurrManager, EnvironmentSatisfaction, RelationshipSatisfaction,  # 16, 17, 18
         TotalWLBScore, TotalPayScore, TotalGrowthScore, TotalEnvScore,  # 19, 20, 21, 22
         Attrition)  # 23

# Convert MaritalStatus into a number.

ds_a$MaritalStatusNum <- as.integer(ds_a$MaritalStatus)  # 24

##############################################
## Model Attrition  (KNN)  Variable test
##############################################
set.seed(5)
# Spit Data set into a training Data set and a testing dataset. @ 70/30
sp = 0.70  # Split percentage

TrainingRows = sample(1:dim(ds_a)[1],round(sp * dim(ds_a)[1])) # Calculate Training Rows
ds_train = ds_a[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
ds_test = ds_a[-TrainingRows,]  # Exclude Training Rows (Testing Rows)

#### Acc = 83.9, Sens = 85.3, Spec = 44
# classifications = knn(ds_train[,c(2,24,19,20,21,22)], ds_test[,c(2,24,19,20,21,22)],
#                       ds_train$Attrition, k = 9, prob = FALSE)

classifications = knn(ds_train[,c(2,6)], ds_test[,c(2,6)],
                      ds_train$Attrition, k = 9, prob = FALSE)


# Other Models #####

# #### Acc = 83.5, Sens = 84.9, Spec = 37.5
# classifications = knn(ds_train[,c(19,20,21,22)], ds_test[,c(19,20,21,22)],
#                       ds_train$Attrition, k = 5, prob = FALSE)
# 
# #### Acc = 83.5, Sens = 84.9, Spec = 37.5
# classifications = knn(ds_train[,c(19,20)], ds_test[,c(19,20)],
#                       ds_train$Attrition, k = 5, prob = FALSE)
# 
# #### Acc = 83.5, Sens = 84.4, Spec = 25.0
# classifications = knn(ds_train[,c(2,4,5,6,8,10,13,14,15,16,17)], ds_test[,c(2,4,5,6,8,10,13,14,15,16,17)],
#                       ds_train$Attrition, k = 5, prob = FALSE)
# 
# 
# #### Acc = 83.5, Sens = 84.4, Spec = 25.0
# classifications = knn(ds_train[,c(2,23,4,5,6,7,8,9,10,11,12,13,14,15,16,17)], ds_test[,c(2,23,4,5,6,7,8,9,10,11,12,13,14,15,16,17)],
#                       ds_train$Attrition, k = 5, prob = FALSE)
# 
# 
# #### Acc = 83.5, Sens = 84.4, Spec = 25.0
# classifications = knn(ds_train[,c(2,23, 5,7, 8,10, 11,13)], ds_test[,c(2,23, 5,7, 8,10, 11,13)],
#                       ds_train$Attrition, k = 5, prob = FALSE)


# classifications ####

table(ds_test$Attrition, classifications)
cm = confusionMatrix(table(ds_test$Attrition, classifications))

AccValue = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + (cm$table[2,1] + cm$table[2,2]))
SensitivityValue = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
SpecifictityValue = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])

AccValue
SensitivityValue
SpecifictityValue


##############################################
## test KNN for best K
## determined K=9 to give the best model.
##############################################

# Variables
set.seed(5)
iter = 100
numks = 10
split = .70

# Create a matrix to hold the values from each run.
AccuMatrix = matrix(nrow = iter, ncol = numks)
SensMatrix = matrix(nrow = iter, ncol = numks)
SpecMatrix = matrix(nrow = iter, ncol = numks)

for(j in 1:iter)
{
  TrainingRows = sample(1:dim(ds_a)[1],round(split * dim(ds_a)[1])) # Calculate Training Rows
  ds_train = ds_a[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
  ds_test = ds_a[-TrainingRows,]  # Exclude Training Rows (Testing Rows)
  for(i in 1:numks)
  {
    
    classifications = knn(ds_train[,c(2,24,19,20,21,22)], ds_test[,c(2,24,19,20,21,22)],
                          ds_train$Attrition, prob = TRUE, k = i)
    
    table(ds_test$Attrition, classifications)
    cm = confusionMatrix(table(ds_test$Attrition, classifications))
    
    AccuMatrix[j,i] = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + (cm$table[2,1] + cm$table[2,2]))
    SensMatrix[j,i] = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
    SpecMatrix[j,i] = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])
  }
  
}

AccuracyMean = colMeans(AccuMatrix)
SpecMean = colMeans(SpecMatrix)
SensMean = colMeans(SensMatrix)

plot(seq(1,numks,1),AccuracyMean, type = "l")
which.max(AccuracyMean)
max(AccuracyMean)


plot(seq(1,numks,1),SpecMean, type = "l")
which.max(SpecMean)
max(SpecMean)


plot(seq(1,numks,1),SensMean, type = "l")
which.max(SensMean)
max(SensMean)


##############################################
## Model Attrition  (Naive Bayes)
## Over 100 iterations Accu = 84.7, Spec = 71.1, Sens = 85.1
## nbm <- naiveBayes(ds_train[,c(2,24,19,22)],ds_train$Attrition)
##############################################

# Variables
set.seed(5)
iter = 100
split = .70

# Create a matrix to hold the values from each run.
AccuVect = vector(length = iter)
SensVect = vector(length = iter)
SpecVect = vector(length = iter)

for(j in 1:iter)
{
  TrainingRows = sample(1:dim(ds_a)[1],round(split * dim(ds_a)[1])) # Calculate Training Rows
  ds_train = ds_a[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
  ds_test = ds_a[-TrainingRows,]  # Exclude Training Rows (Testing Rows)
  
  nbm <- naiveBayes(ds_train[,c(2,24,19,22)],ds_train$Attrition)
  
  # Predict outcomes for Testing data set.
  ds_test$predict_outcome = predict(nbm,ds_test)
  
  classifications = predict(nbm,ds_test)
  # classifications
  table(ds_test$Attrition, classifications)
  cm = confusionMatrix(table(ds_test$Attrition, classifications))
  
  AccuVect[j] = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + (cm$table[2,1] + cm$table[2,2]))
  SensVect[j] = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
  SpecVect[j] = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])
}

mean(AccuVect)
mean(SpecVect)
mean(SensVect)

plot(seq(1,length(AccuVect),1),AccuVect, type = "l", main = "Accuracy From NB Model", xlab = "Iteration", ylab = "Accuracy")

plot(seq(1,length(SpecVect),1),SpecVect, type = "l", main = "Specificity From NB Model", xlab = "Iteration", ylab = "Specificity")

plot(seq(1,length(SensVect),1),SensVect, type = "l", main = "Sensitivity From NB Model", xlab = "Iteration", ylab = "Sensitivity")



# #### Acc = 88.1, Sens = 89.5, Spec = 72.7
# nbm <- naiveBayes(ds_train[,c(2,23,18,19,20,21)],ds_train$Attrition) 
# 
# #### Acc = 86.2, Sens = 85.9, Spec = 100
# nbm <- naiveBayes(ds_train[,c(2,23,18,21)],ds_train$Attrition) 
# 
# #### Acc = 85.8, Sens = 85.8, Spec = 83.3
# nbm <- naiveBayes(ds_train[,c(18,19,20,21)],ds_train$Attrition) 



##############################################
## Model Monthly Income (Linear Regression)
##############################################


# attData %>% ggplot(aes(y = MonthlyIncome, x = Age)) + geom_point() + geom_smooth()

attData %>% ggplot(aes(y = MonthlyIncome, x = JobLevel)) + geom_point() + geom_smooth()

attData %>% ggplot(aes(y = MonthlyIncome, x = JobRole)) + geom_boxplot()

# attData %>% ggplot(aes(y = MonthlyIncome, x = TotalWorkingYears)) + geom_point() + geom_smooth()

attData %>% ggplot(aes(y = MonthlyIncome, x = YearsAtCompany)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = YearsInCurrentRole)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = YearsSinceLastPromotion)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = PayScore)) + geom_point() + geom_smooth()

attData %>% ggplot(aes(y = MonthlyIncome, x = PayScore)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = RaiseScore)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = StockScore)) + geom_point() + geom_smooth()

attData %>% ggplot(aes(y = MonthlyIncome, x = RoleScore)) + geom_point() + geom_smooth()

# attData %>% ggplot(aes(y = MonthlyIncome, x = TotalGrowthScore)) + geom_point() + geom_smooth()


#  RMSE = 3596.48
#  MI_fit <- lm(MonthlyIncome~YearsAtCompany+Age, data = ds_train)

#  RMSE = 1514.68
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age+YearsAtCompany+TotalWorkingYears+YearsInCurrentRole+YearsSinceLastPromotion, data = ds_train)

#  RMSE = 1511.81
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age+YearsAtCompany+TotalWorkingYears+YearsInCurrentRole, data = ds_train)

#  RMSE = 1511.20
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age+YearsAtCompany+TotalWorkingYears, data = ds_train)

#  RMSE = 1503.37
#  MI_fit <- lm(MonthlyIncome~JobLevel, data = ds_train)

#  RMSE = 1499.20
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age+TotalWorkingYears, data = ds_train)

#  RMSE = 1486.32
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age+YearsAtCompany, data = ds_train)

#  RMSE = 1482.76
#  MI_fit <- lm(MonthlyIncome~JobLevel+Age, data = ds_train)

#  RMSE = 620.65
#  MI_fit <- lm(MonthlyIncome~Age+JobLevel+TotalWLBScore+PayScore+TotalGrowthScore+TotalEnvScore, data = ds_train)

#  RMSE = 616.39
#  MI_fit <- lm(MonthlyIncome~Age+JobLevel+PayScore+TotalGrowthScore, data = ds_train)

#  RMSE = 615.51
#  MI_fit <- lm(MonthlyIncome~JobLevel+PayScore+TotalGrowthScore, data = ds_train)

#  RMSE = 453.28
#  MI_fit <- lm(MonthlyIncome~JobLevel+JobRole+NumCompaniesWorked+YearsAtCompany+PayScore+TotalGrowthScore, data = ds_train)

#  RMSE = 452.93
#  MI_fit <- lm(MonthlyIncome~JobLevel+JobRole+YearsAtCompany+PayScore+TotalGrowthScore, data = ds_train)

#  RMSE = 440.33
#  MI_fit <- lm(MonthlyIncome~JobLevel+JobRole+YearsAtCompany+PayScore+RoleScore, data = ds_train)

# Variables
set.seed(3)
iter = 100

# Create a matrix to hold the values from each run.
ObserVect = vector(length = iter)
PredVect = vector(length = iter)

for(j in 1:iter)
{
  TrainingRows = sample(1:dim(attData)[1],dim(attData)[1]-1) # Calculate Training Rows (leave one out)
  ds_train = attData[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
  ds_test = attData[-TrainingRows,]  # Exclude Training Rows (Testing Rows)
  
  MI_fit <- lm(MonthlyIncome~JobLevel+JobRole+YearsAtCompany+PayScore+RoleScore, data = ds_train)
  ObserVect[j] <- ds_test$MonthlyIncome
  PredVect[j] <- predict(MI_fit, newdata = ds_test)
  
}

mi_model_df <- data.frame(ObserVect, PredVect)
# Calculate Resisduals
mi_model_df$Res <- mi_model_df$ObserVect - mi_model_df$PredVect
# Square the Residuals
mi_model_df$ResSQ = mi_model_df$Res^2
# Calculate the RMSE
sqrt(mean(mi_model_df$ResSQ))
  
summary(MI_fit)



#beta_0
MI_fit$coefficients[1]
#beta_1
MI_fit$coefficients[2]

#Confidence Intervals
confint(MI_fit)



hist(MI_fit$resid, main = "Histogram of Residuals", xlab = "Residuals")


# qqnorm(MI_fit$resid)
# qqline(MI_fit$resid)







