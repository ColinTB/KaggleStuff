rm(list=ls())
options(scipen=999)

# Program Details: Overview -----------------------------------------------
# *************************************************************************
# Programmer:		Colin Turner Bloom
# Case:				  Kaggle Titanic
# Program:      cr_
# Version:      1
# Date Created: 5/8/2017         
# Last Updated:	5/8/2017	

# Description: 
#   Intro Kaggle Competition

# Version Notes: 

# Audited:		[Auditor's Name]
# *************************************************************************

# packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(magrittr)

# Directories -------------------------------------------------------------

main <- "H:/CBloom/Kaggle/Titanic"
input <- file.path(main,"Input")
intermediate <- file.path(main,"Intermediate")
output <- file.path(main,"Output")

# Load Data ---------------------------------------------------------------

train.raw <- read_csv(file.path(input, "train.csv"), col_types = cols())
test.raw <- read_csv(file.path(input, "test.csv"), col_types = cols())



# Cleaning ----------------------------------------------------------------

cleaning_procedure <- . %>% 
  group_by(Sex, Pclass) %>% 
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = T), Age),
         Cabin = ifelse(is.na(Cabin), "unknown", Cabin)) %>% 
  ungroup()
  
train <- train.raw %>% cleaning_procedure

# Modelling ---------------------------------------------------------------

model <- glm(Survived ~ Pclass + Sex + Age + Embarked + SibSp + Parch, family = binomial(link = 'logit'), data = train)


examine <- train %>% 
  mutate(Survived2 = predict.glm(model, train, type = "response"),
         Survived3 = ifelse(Survived2 > 0.5 | is.na(Survived2), 1, 0))

examine %$% table(Survived, Survived3)

# Training Score

sum(examine$Survived == examine$Survived3, na.rm = T) / nrow(examine)



# Results -----------------------------------------------------------------

results <- test.raw %>% 
  cleaning_procedure() %>% 
  mutate(Survived = predict.glm(model, ., type = "response"),
         Survived = ifelse(Survived > 0.5 | is.na(Survived), 1, 0)) %>% 
  select(PassengerId, Survived)

write_csv(results, file.path(output, "kaggle_titanic_submission2.csv"))







