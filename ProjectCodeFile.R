##############################################
## READ IN Libraries
##############################################
library(dplyr)



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
    
    # If overtime elegible could go either way. And Salary employees are usually expected to put in extra hours.
    df$OvertimeScore[i] = if(df$OverTime[i] == "Yes"){0} else{-1}  
    
    df$TotalWLBScore[i] = df$TravelScore[i] + df$DistanceScore[i] + df$OvertimeScore[i] + df$WorkLifeBalance[i]
    
    
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
    
    
    #####  Enviormental Score  #####
    
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






fifaEDA2 %>% select(Overall_Ability, Aggression, Penalties) %>% 
  ggpairs(mapping = aes(color = Overall_Ability), title = "Overall Ability Score and Aggression & Penalties") 