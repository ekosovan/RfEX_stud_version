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

install.packages("tidyverse")#I guess that tidyverse has some packages already (dplyr, tidyr)
install.packages("haven")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("stringr")

library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(stringr)

# Further description is available in adult.names file

# 1) read the data, make sure columns are read in the right format, i.e. character, double, etc.
adult_data <- read_csv("adult_data", col_names = c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","salary"))
adult_data<- as.data.frame(adult_data)
str(adult_data)

adult_names <- read_csv("adult_names")
adult_names<- as.data.frame(adult_names)
head(adult_names)
# 2) How many men and women are represented in the dataset? 
adult_data %>% count(sex)
  #There are 10771 Females and 21790 Males

# 3) What is the average age of women
adult_data %>% filter(sex=="Female") %>%
  summarize(mean=mean(age))

#Average age for Female is 36.86 years.

# 4) What are the mean and standard deviation of age for those who earn more than 50K per year
# and those who earn less than 50K per year

adult_data %>% group_by(salary) %>%
  summarize(mean=mean(age),std=sd(age))

#Average age for the one who earn less than 50K/year is 36.8 years, std 14.0
#Average age for the one who earn more than 50K/year is 44.2 years, std 10.5

# 5) Is it true that people who earn more than 50K have at least high school education? (education - 
#     Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters or Doctorate feature)

adult_data %>% filter(salary==">50K") %>%
  count(education)

#Not true, there are cases to have education under.

# 6) Display age statistics for each race and each gender. Find the maximum age of men of 
# Amer-Indian-Eskimo race

adult_data %>% group_by(race,sex) %>%
  summarize(mean=mean(age), max=max(age),min=min(age),median=median(age))
#The maximum age of men of Amer Indian Eskimo race is 80 years.  

# 7) Among whom is the proportion of those who earn a lot (>50K) is greater: married or single men?
#    Consider as married those who have marital-status starting with Married (Married-civ-spouse,
#    Married-spouse-absent or Married-AF-spouse)
# NOTE: this is regex question ;)

x <- adult_data %>% filter(salary==">50K")

y <- str_count(x$`marital-status`,"Married{1}")
married <- sum(y)
single <- length(x$`marital-status`)-married

proportion <- married/length(x$`marital-status`)

#There are 86% of married people who earn more than 50K.

# 8) What is the maximum number of hours a person works per week? How many people work such a number
#    of hours, and what is the percentage of those who earn a lot (>50K) among them?

adult_data %>% filter(`hours-per-week`==max(`hours-per-week`)) %>%
  count(salary) %>%
  mutate(split=n/sum(n)*100)

#There is 29.4% of those who earn more than 50K. 

# 9) Count the average time of work for those who earn a little and a lot (salary) for 
#    each country (native-country). What will these be in Japan?

adult_data %>% group_by(`native-country`, salary)%>%
  summarize(mean=mean(`hours-per-week`))%>%
  filter(`native-country`=="Japan")

#The average time of work in Japan for who earn little is 41 hours per week and 48 hours for that who earn a lot.

##############################################################################
# Problem 2 (30%)
##############################################################################


# This is a very simple problem, yet it is worth 30% of the PS grade
# Load titanic dataset. EXPLORE IT, if needed. Find the most common female name
# and create a sorted dataset with columns "female name" and "number of occurences", i.e.
# how many times the given name occured in the dataset.

rm(list=ls()) #cleaning environment to make it more transparent for you

titanic <- read_csv("titanic.csv")
glimpse(titanic) #I use this to quickly check on the structure of the data
head(titanic) #This I use to get the idea about the info stored there


female <- titanic %>% filter(Sex=="female") %>% select(Name)
vector1 <- str_extract(female$Name,("(?<=\\.)\\s\\w+")) #extracting first name after title
vector2 <- str_extract(female$Name,("(?<=\\()\\w+")) #extracting first names in the bracket

for (j in 1:length(vector1)){
  if (is.na(vector2[j])==TRUE){
    vector2[j]=vector1[j]
  }
}

vector2 <- str_remove(vector2,"^\\s")
vector2 <- as.data.frame(vector2)
names(vector2) <- c("Name")
vector2 %>% count(Name)%>%arrange(desc(n))

#the most common name is Anna (15 observations)
