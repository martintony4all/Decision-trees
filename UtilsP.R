##################
# This is a combination of functions for pre-processing 
# the datasets used in ISQA 8720 project. 
# 
# Author: Group 1
##################

library(dplyr)

# this function prepares the titanic dataset for our course
# it converts some of the integer columns to factors, adds the NA as a level for the 'embarked' variable,
# and relevels the target variable such that the positive class is used correctly in classification
prepare_customer <- function(customer, target_variable = 'churned', positive_class = '1'){
  
  # convert variables into factors
  customer <- customer %>% mutate_at(c('churned', 'HQ_Country2','NAICS2.2', 'NAICS3.3'), as.factor)
  customer <- customer %>% mutate_if(is.character, as.factor)
  
  # then, we convert the 0/1 target variable into no/yes, as 0/1 can lead to problems when using as the dependent variable
  customer <- customer %>% mutate(Income, Income = recode(churned, '0' = 'No', '1' = 'Yes'))
  
  # add the NA as factor level to all factor variables
  customer <- customer %>% mutate_if(~ any(is.na(.)), addNA)
  
  return(customer)
}
