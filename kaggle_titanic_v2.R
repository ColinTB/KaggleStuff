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
#   Intro Kaggle Competition for the titanic data
#   Trying to push change to github

# Version Notes: 

# Audited:		[Auditor's Name]
# *************************************************************************

# packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(magrittr)
library(forcats)
library(randomForest)

# Directories -------------------------------------------------------------

main <- "H:/CBloom/Kaggle/Titanic"
input <- file.path(main,"Input")
intermediate <- file.path(main,"Intermediate")
output <- file.path(main,"Output")

# Load Data ---------------------------------------------------------------

train.raw <- read_csv(file.path(input, "train.csv"), col_types = cols()) %>% mutate(train = T)
test.raw <- read_csv(file.path(input, "test.csv"), col_types = cols()) %>% mutate(train = F)



# Cleaning ----------------------------------------------------------------

all.data <- train.raw %>%
  bind_rows(test.raw) %>% 
  mutate(title = str_extract(Name, "(?<=, )[[:alpha:]]+"),
         title = as.character(fct_lump(title, 4)),
         fam.size = SibSp + Parch + 1,
         deck = str_extract(Cabin, "^[[:alpha:]]")) %>% 
  group_by(Sex, Pclass) %>% 
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = T), Age),
         Cabin = ifelse(is.na(Cabin), "unknown", Cabin),
         deck = ifelse(is.na(deck), "unknown", deck),
         kid = Age < 16,
         mom = Age > 22 & Sex == "female" & Parch >= 1) %>% 
  ungroup() %>% 
  mutate_if(is.character, funs(str_replace_na(.))) %>% 
  mutate_each(funs(factor(.)), Sex, Cabin, Embarked, title, deck) 
  
train <- all.data %>% filter(train)
test <- all.data %>% filter(!train)

# Modelling ---------------------------------------------------------------

model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Embarked + SibSp + Parch + title + fam.size + deck + kid + mom, data = train)


examine <- train %>% 
  mutate(Survived2 = predict(model, .))

examine %$% table(Survived, Survived2)

# Training Score

sum(examine$Survived == examine$Survived2, na.rm = T) / nrow(examine)



# Results -----------------------------------------------------------------

results <- test %>% 
  mutate(Survived = predict(model, .)) %>% 
  select(PassengerId, Survived)

write_csv(results, file.path(output, "kaggle_titanic_submission4.csv"))



old.results <- read_csv(file.path(output, "kaggle_titanic_submission2.csv"))


compare <- results %>% 
  left_join(old.results, by = "PassengerId")

compare %$% table(Survived.x, Survived.y)


