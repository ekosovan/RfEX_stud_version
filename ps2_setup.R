##############################################################################
# Problem Set 2
##############################################################################

##############################################################################
# Instructions
##############################################################################
# 1) You should submit the .R script with the solution
# 2) Make sure to comment wherever it is needed
# 3) Every student should submit his\hers own copy


##############################################################################
# Problem 1 (70%)
##############################################################################

# You'll be working with adult.data . Here's a brief description of the 
# variables:
# age: continuous.
# workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, 
#            Without-pay, Never-worked.
# fnlwgt: continuous.
# education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc,
#            9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
# education-num: continuous.
# marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
# occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
# relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
# race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
# sex: Female, Male.
# capital-gain: continuous.
# capital-loss: continuous.
# hours-per-week: continuous.
# native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.
# salary: >50K,<=50K

# Further description is available in adult.names file

# 1) read the data, make sure columns are read in the right format, i.e. character, double, etc.
install.packages(c('jsonlite','readxl', 'writexl','purrr', 'foreign'))

library(magrittr)
library(tidyverse)
library(purrr)
library(foreign)
setwd("C:/Users/Michal/Documents/GitHub/RfEX_stud_version/ps2")
adult <- read.csv("~/GitHub/RfEX_stud_version/ps2/adult.data", header=FALSE)
as.data.frame(adult)
adult_names <- read.csv("~/GitHub/RfEX_stud_version/ps2/adult.names", header=FALSE)


# 2) How many men and women are represented in the dataset?  32561W, 32560M
num_female <- adult %>% select(V10)
df <- data.frame(matrix(unlist(num_female), nrow=length(num_female), byrow=F))
df <- t(df)
df <- as.data.frame(df)

num_female <- adult %>% select(V10) %>%
  filter(V10 == " Female") %>%
  nrow(.) %>%
  print()
 

num_male <- adult %>% select(V10) %>%
  filter(V10 == " Male") %>%
  nrow(.) %>%
  print()

# 3) What is the average age of women
female_age <- adult %>% select(V1, V10) %>%
  filter(V10 == " Female") %>%
  na.omit() %>%
  select(V1) %>%
  summarise(mean(V1, na.rm = T)) %>%
  print()

# 4) What are the mean and standard deviation of age for those who earn more than 50K per year
# and those who earn less than 50K per year
mean_rich <- adult %>% select(V1, V15) %>%
  filter(V15 == " >50K") %>%
  select(V1) %>%
  summarise(mean(V1, na.rm = T)) %>%
  print()

mean_poor <- adult %>% select(V1, V15) %>%
  filter(V15 == " <=50K") %>%
  select(V1) %>%
  summarise(mean(V1, na.rm = T)) %>%
  print()

sd_rich <- adult %>% select(V1, V15) %>%
  filter(V15 == " >50K") %>%
  select(V1) %>%
  summarise(sd(V1, na.rm = T)) %>%
  print()

sd_poor <- adult %>% select(V1, V15) %>%
  filter(V15 == " <=50K") %>%
  select(V1) %>%
  summarise(sd(V1, na.rm = T)) %>%
  print()

# 5) Is it true that people who earn more than 50K have at least high school education? (education - 
#     Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters or Doctorate feature)
education <- adult %>% select(V4, V15) %>%
    filter(V15 == " >50K")

table(education)
sort(table(education),decreasing=TRUE)

table(adult[which(adult$V15==" >50K")])
#HOW TO SORT IN TABLE TO PRESERVE EDU NAMES?

# 6) Display age statistics for each race and each gender. Find the maximum age of men of 
# Amer-Indian-Eskimo race
adult %>% 
  rename(
    Age = V1,
    Race = V9,
    Gender = V10
  )

age_statistics <- adult %>% select(V9, V10, V1)
table(age_statistics)

Amer_Ind_Esk <- adult %>% select(V1, V9) %>%
        filter(V9 == " Amer-Indian-Eskimo") %>%
        summarise(max(V1, na.rm = T)) %>%
        print()
  
# 7) Among whom is the proportion of those who earn a lot (>50K) is greater: married or single men?
#    Consider as married those who have marital-status starting with Married (Married-civ-spouse,
#    Married-spouse-absent or Married-AF-spouse)
# NOTE: this is regex question ;)
married1 <- adult %>% select(V10, V15, V6) %>%
          filter(V10 == " Male", V15 == " >50K") %>%
          filter(V6 == " Married-civ-spouse") %>%
          nrow %>%
          print()

married2 <- adult %>% select(V10, V15, V6) %>%
  filter(V10 == " Male", V15 == " >50K") %>%
  filter(V6 == " Married-spouse-absent") %>%
  nrow %>%
  print()  

married3 <- adult %>% select(V10, V15, V6) %>%
  filter(V10 == " Male", V15 == " >50K") %>%
  filter(V6 == " Married-AF-spouse") %>%
  nrow %>%
  print()  

married = married1 + married2 + married3

all <- adult %>% select(V10, V15) %>%
  filter(V10 == " Male", V15 == " >50K") %>%
  nrow %>%
  print()

single = all - married

if (married > single) {
  print("Proportion of married is greater")
} else {
  print("Proportion of single is greater")
}
# 8) What is the maximum number of hours a person works per week? How many people work such a number
#    of hours, and what is the percentage of those who earn a lot (>50K) among them?
attach(adult)
max(V13, na.rm=T)

max_work_H <- adult %>% select(V13, V15) %>%
        filter(V13 == "99") %>%
        nrow() %>%
        print 

rich_perc <- adult %>% select(V13, V15) %>%
  filter(V13 == "99") %>%
  filter(V15 == " >50K") %>%
  nrow() %>%
  print

percentage_workers = (rich_perc / max_work_H)*100
percentage_workers
#Max num of hours is 99. 85 people work such a num of hours and it is 29.41%. 

# 9) Count the average time of work for those who earn a little and a lot (salary) for 
#    each country (native-country). What will these be in Japan?


##############################################################################
# Problem 2 (30%)
##############################################################################


# This is a very simple problem, yet it is worth 30% of the PS grade
# Load titanic dataset. EXPLORE IT, if needed. Find the most common female name
# and create a sorted dataset with columns "female name" and "number of occurences", i.e.
# how many times the given name occured in the dataset.
library(stringr)
titanic  <- read.csv("C:/Users/Michal/Desktop/MAE in Applied Economics/R for Econometrics/ps2/titanic.csv")
as.data.frame(titanic)

clean1 <- titanic %>% select(Sex, Name) %>%
      filter(Sex=="female") %>%
      print()

clean2 <-str_extract(clean1$Name, ("(?<=Miss.)\\s[:alpha:]+|(?<=\\()[:alpha:]+"))

clean3 <- str_remove(clean2,"^\\s")

colnames(clean3)
clean4 <- data.frame(clean3)
colnames(clean4) <- "Name"

clean5 <- group_by(clean4, Name)
clean6 <-summarise(clean5, count=n())
clean7<-clean6[order(-clean6$count),]
clean7

# The name is Anna - 15 observations. There are also 8 NA values which have no influence of the results 
