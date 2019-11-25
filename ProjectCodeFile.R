##############################################
## READ IN Libraries
##############################################

library(dplyr)
library(caret)
library(class)
library(e1071)
library(ggplot2)
library(GGally)

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
## EDA for Work Life Balance variables
##############################################
# The work Life Balance factors do not seem to have much of an impact to determine attrition.
attData %>% select(Attrition, BusinessTravel, DistanceFromHome, WorkLifeBalance) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")
  
attData %>% select(Attrition, BusinessTravel) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")

attData %>% select(Attrition, DistanceFromHome) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")

attData %>% select(Attrition, WorkLifeBalance) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")

# My score for Work Life Balance seems normally distributed and might be better to determine attrition.
attData %>% select(Attrition, TotalWLBScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Work Life Balance factors vs Attrition")


##############################################
## EDA for Compensation variables
##############################################
# Monthly Income seems to be a good indicatior for determining attrition.
attData %>% select(Attrition, MonthlyIncome, PercentSalaryHike, StockOptionLevel) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Compensation factors vs Attrition")

# The Total Pay Score seems normally distributed and a good indicator of determining attrition.
attData %>% select(Attrition, TotalPayScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Compensation factors vs Attrition")


##############################################
## EDA Career Growth variables
##############################################
# Job Level seems to be a good indicator for determining attrition, but the other variables not so much.
attData %>% select(Attrition, JobLevel, TrainingTimesLastYear, YearsSinceLastPromotion, YearsInCurrentRole) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Career Growth factors vs Attrition")
  
# Total Growth score seems like a good indicator of attrition, but is not normally distributed.
attData %>% select(Attrition, TotalGrowthScore) %>%
  ggpairs(mapping = aes(color = Attrition), title = "Career Growth factors vs Attrition")


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
  ggpairs(mapping = aes(color = Attrition), title = "Environmental factors vs Attrition")

  
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
##############################################  c(5,7, 8,10, 11,13)]

ds_full <- attData %>%
  select(ID,
         Age, MaritalStatus, Education,   # 2, 3, 4
         DistanceFromHome, WorkLifeBalance, TravelScore, # 5, 6, 7
         MonthlyIncome, PercentSalaryHike, StockOptionLevel, JobLevel, # 8, 9, 10, 11
         TrainingTimesLastYear, YearsInCurrentRole, YearsSinceLastPromotion,  # 12, 13, 14
         YearsWithCurrManager, EnvironmentSatisfaction, RelationshipSatisfaction,  # 15, 16, 17
         TotalWLBScore, TotalPayScore, TotalGrowthScore, TotalEnvScore,  # 18, 19, 20, 21
         Attrition) # 22

# Convert MaritalStatus into a number.

ds_full$MaritalStatusNum <- as.integer(ds_full$MaritalStatus)  # 23

##############################################
## Model Attrition  (KNN)
##############################################

# Spit Data set into a training Data set and a testing dataset. @ 70/30
sp = 0.70  # Split percentage

TrainingRows = sample(1:dim(ds_full)[1],round(sp * dim(ds_full)[1])) # Calculate Training Rows
ds_train = ds_full[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
ds_test = ds_full[-TrainingRows,]  # Exclude Training Rows (Testing Rows)



#### Acc = 83.9, Sens = 85.3, Spec = 44
classifications = knn(ds_train[,c(2,23,18,19,20,21)], ds_test[,c(2,23,18,19,20,21)],
                      ds_train$Attrition, k = 5, prob = FALSE)

#### Acc = 83.5, Sens = 84.9, Spec = 37.5
classifications = knn(ds_train[,c(18,19,20,21)], ds_test[,c(18,19,20,21)],
                      ds_train$Attrition, k = 5, prob = FALSE)

#### Acc = 83.5, Sens = 84.9, Spec = 37.5
classifications = knn(ds_train[,c(18,19)], ds_test[,c(18,19)],
                      ds_train$Attrition, k = 5, prob = FALSE)

#### Acc = 83.5, Sens = 84.4, Spec = 25.0
classifications = knn(ds_train[,c(2,4,5,6,8,10,13,14,15,16,17)], ds_test[,c(2,4,5,6,8,10,13,14,15,16,17)],
                      ds_train$Attrition, k = 5, prob = FALSE)


#### Acc = 83.5, Sens = 84.4, Spec = 25.0
classifications = knn(ds_train[,c(2,23,4,5,6,7,8,9,10,11,12,13,14,15,16,17)], ds_test[,c(2,23,4,5,6,7,8,9,10,11,12,13,14,15,16,17)],
                      ds_train$Attrition, k = 5, prob = FALSE)


#### Acc = 83.5, Sens = 84.4, Spec = 25.0
classifications = knn(ds_train[,c(2,23, 5,7, 8,10, 11,13)], ds_test[,c(2,23, 5,7, 8,10, 11,13)],
                      ds_train$Attrition, k = 5, prob = FALSE)


# classifications

table(ds_test$Attrition, classifications)

cm = confusionMatrix(table(ds_test$Attrition, classifications))

AccValue = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + 
                                                  (cm$table[2,1] + cm$table[2,2]))

# MisClassValue = ((cm$table[1,2] + cm$table[2,1])) / ((cm$table[1,1] + cm$table[1,2]) + 
#                                                        (cm$table[2,1] + cm$table[2,2]))

SensitivityValue = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
SpecifictityValue = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])

cm
AccValue
# MisClassValue
SensitivityValue
SpecifictityValue


##############################################
## Model Attrition  (Naive Bayes)
##############################################

# # Spit Data set into a training Data set and a testing dataset. @ 70/30
# sp = 0.70  # Split percentage
# 
# TrainingRows = sample(1:dim(ds_full)[1],round(sp * dim(ds_full)[1])) # Calculate Training Rows
# ds_train = ds_full[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
# ds_test = ds_full[-TrainingRows,]  # Exclude Training Rows (Testing Rows)


#### Acc = 88.1, Sens = 89.5, Spec = 72.7
nbm <- naiveBayes(ds_train[,c(2,23,18,19,20,21)],ds_train$Attrition) 

#### Acc = 86.2, Sens = 85.9, Spec = 100
nbm <- naiveBayes(ds_train[,c(2,23,18,21)],ds_train$Attrition) 

#### Acc = 85.8, Sens = 85.8, Spec = 83.3
nbm <- naiveBayes(ds_train[,c(18,19,20,21)],ds_train$Attrition) 


# Predict outcomes for Testing data set.
ds_test$predict_outcome = predict(nbm,ds_test)

classifications = predict(nbm,ds_test)
# classifications
table(ds_test$Attrition, classifications)
cm = confusionMatrix(table(ds_test$Attrition, classifications))

AccuracyValue = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + 
                                                       (cm$table[2,1] + cm$table[2,2]))

# MisClassValue = ((cm$table[1,2] + cm$table[2,1])) / ((cm$table[1,1] + cm$table[1,2]) + 
#                                                        (cm$table[2,1] + cm$table[2,2]))

SensitivityValue = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
SpecifictityValue = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])

table(ds_test$Attrition, classifications)
AccuracyValue
# MisClassValue
SensitivityValue
SpecifictityValue




# Plot the results
ds_test %>% ggplot(mapping = aes(x= ds_test$Attrition, fill = ds_test$predict_outcome))+
  geom_bar()+
  labs(title="Survivability by Class (NB Model)", x="Class", y="Count", fill="")



##############################################
## Model Monthly Income (Linear Regression)
##############################################














