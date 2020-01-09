##############################################################################
# Problem Set 2 
# caret package for machine learning 
# there is variable SEX binary for the columns
##############################################################################
install.packages("stringr")
install.packages("readr")
install.packages("haven")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")


##############################################################################
# Instructions
##############################################################################
# 1) You should submit the .R script with the solution
# 2) Make sure to comment wherever it is needed
# 3) Every student should submit his\hers own copy

library(tidyverse)
library(haven)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library('magrittr')

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
#raw_data = read.table("C:/Users/Alex/Documents/CERGE-EI study/R for econometrics/ALL FILES FROM ZHENYA AT 19 11 2019/ps2/adult.data", fileEncoding="UTF-8", dec=",")
setwd("C:/Users/Alex/Documents/CERGE-EI study/R for econometrics/ALL FILES FROM ZHENYA AT 19 11 2019/ps2/")
raw_data = read_csv("adult.data", col_names = c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","salary"))
raw_data = as.data.frame(raw_data)
str(raw_data)


# 2) How many men and women are represented in the dataset? 
# Method 1
summary(raw_data$sex)
cat("No of Females = ", nrow(raw_data[raw_data$sex == "Female", ]))
cat("No of Males = ", nrow(raw_data[raw_data$sex == "Male", ]))
# lenght received by summary: 32561 - total number of observations
# No of Females =  10771; No of Males =  21790
32561==10771+21790

# method 2 - excluded from execution but still working :)
# iffemale = raw_data$dummyiffemale = ifelse(raw_data$sex == "Female", 1, 0)
# sum(iffemale)


# 3) What is the average age of women
raw_data %>% filter(sex=="Female") %>%
  summarize(mean_age=mean(age))
# 4) What are the mean and standard deviation of age for those who earn more than 50K per year
# and those who earn less than 50K per year
raw_data %>% group_by(salary) %>%
  summarize(mean_age=mean(age),standard_deviation=sd(age))
# 5) Is it true that people who earn more than 50K have at least high school education? (education - 
#     Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters or Doctorate feature)
raw_data %>% filter(salary==">50K") %>%
  count(education)
# output
#education        n
#<chr>        <int>
#  1 10th            62
#2 11th            60
#3 12th            33
#4 1st-4th          6
#5 5th-6th         16
#6 7th-8th         40
#7 9th             27
#8 Assoc-acdm     265
#9 Assoc-voc      361
#10 Bachelors     2221
#11 Doctorate      306
#12 HS-grad       1675
#13 Masters        959
#14 Prof-school    423
#15 Some-college  1387

# ANSWER: THERE ARE MANY CASES OF PEOPLE RECEIVING MORE THAN 50000 USD 
# WITH MINIMUM EDUCATION, NOT WITH HIGH SCHOOL

# 6) Display age statistics for each rate and each gender. Find the maximum age of men of 
# Amer-Indian-Eskimo race
raw_data %>% group_by(race,sex) %>%
  summarize(mean=mean(age), max=max(age),min=min(age),median=median(age))
#race               sex     mean   max   min median
#<chr>              <chr>  <dbl> <dbl> <dbl>  <dbl>
#  1 Amer-Indian-Eskimo Female  37.1    80    17     36
#2 Amer-Indian-Eskimo Male    37.2    82    17     35
#3 Asian-Pac-Islander Female  35.1    75    17     33
#4 Asian-Pac-Islander Male    39.1    90    18     37
#5 Black              Female  37.9    90    17     37
#6 Black              Male    37.7    90    17     36
#7 Other              Female  31.7    74    17     29
#8 Other              Male    34.7    77    17     32
#9 White              Female  36.8    90    17     35
#10 White              Male    39.7    90    17     38

# Find the maximum age of men of Amer-Indian-Eskimo race: it is 82 years old

# 7) Among whom is the proportion of those who earn a lot (>50K) is greater: married or single men?
#    Consider as married those who have marital-status starting with Married (Married-civ-spouse,
#    Married-spouse-absent or Married-AF-spouse)
# NOTE: this is regex question ;)
richer = raw_data %>% filter(salary==">50K")

married = str_count(richer$`marital_status`,"Married{1}")
richer
married
married1 = sum(married)
married1
single = length(richer$`marital_status`)-married1
single


earn_lot_married = married1/length(richer$`marital_status`)
#ANSWER: We have 0.8590741 (85.9%) of married people who earn a lot; therefore, 14.1% of single people earning a lot

total_earn_more50 = married1 + single
total_earn_more50
earn_lot_married
1105/6736
1105/total_earn_more50
1-1105/total_earn_more50
# same number: 14.1% of those earning a lot; 0.859% people earn little

# 8) What is the maximum number of hours a person works per week? How many people work such a number
#    of hours, and what is the percentage of those who earn a lot (>50K) among them?
max(raw_data$hours_per_week)
# What is the maximum number of hours a person works per week? - it is 99 hours per week
# list of those people who work that much is represented above
raw_data %>% filter(`hours_per_week`==max(`hours_per_week`))
raw_data %>% filter(`hours_per_week`==max(`hours_per_week`))%>%
  count(salary) %>%
  mutate(percent_owerworking_people=100*n/sum(n))
# output: 
#salary     n               percent_owerworking_people
#<chr>  <int>                      <dbl>
# <=50K     60                       70.6
# >50K      25                       29.4   (those who earn more than 50 000 USD)

# ANSWER: 29.4% (those who earn more than 50 000 USD)

# 9) Count the average time of work for those who earn a little and a lot (salary) for 
#    each country (native-country). What will these be in Japan?

raw_data %>% group_by(`native_country`, salary)%>%
  summarize(mean=mean(`hours_per_week`))%>%
  filter(`native_country`=="Japan")

#native_country     salary    mean
#<chr>              <chr>     <dbl>
#  1 Japan          <=50K      41  
#  2 Japan          >50K      48.0

#41 hours per week work Japanese with salaries lower than 50 000 USD
# 48 hours per week work Japanese with salaries higher than 50 000 USD


##############################################################################
# Problem 2 (30%)
##############################################################################


# This is a very simple problem, yet it is worth 30% of the PS grade
# Load titanic dataset. EXPLORE IT, if needed. Find the most common female name
# and create a sorted dataset with columns "female_name" and "number of occurences", i.e.
# how many times the given name occured in the dataset.

tytanic = read_csv("titanic.csv")
summary(tytanic)
head(tytanic)
# this seems to be the most sad database among all 
tytanic$Name[1]
# If you look back at the output of our inquiry on Owen, his name is still encoded as a factor. 
# We need character - factor makes no sense
tytanic$Name <- as.character(tytanic$Name)
strsplit(tytanic$Name[1], split='[,.]')
strsplit(tytanic$Name[1], split='[,.]')[[1]][2]
woman = tytanic %>% filter(Sex=="female") %>% select(Name)
woman
first_name_extracted_from_title <- str_extract(woman$Name,("(?<=\\.)\\s\\w+"))
first_name_extracted_from_the_brackets <- str_extract(woman$Name,("(?<=\\()\\w+"))
first_name_extracted_from_title
first_name_extracted_from_the_brackets
for (i in 1:length(first_name_extracted_from_title)){
  if (is.na(first_name_extracted_from_the_brackets[i])==TRUE){
    first_name_extracted_from_the_brackets[i]=first_name_extracted_from_title[i]
  }
}
first_name_extracted_from_the_brackets <- str_remove(first_name_extracted_from_the_brackets,"^\\s")
first_name_extracted_from_the_brackets <- as.data.frame(first_name_extracted_from_the_brackets)
names(first_name_extracted_from_the_brackets) <- c("Female_name")
first_name_extracted_from_the_brackets %>% count(Female_name)%>%arrange(desc(n))
# The most popular women names are: Anna (15 times); Mary (14 times); Elizabeth (11 times); Margaret (10 times); all the other women names are reflected less than 7 times
#Female_name     n
#<fct>       <int>
#  1 Anna           15
#2 Mary           14
#3 Elizabeth      11
#4 Margaret       10
#5 Alice           6
#6 Bertha          5
#7 Helen           5
#8 Maria           5
#9 Ada             4
#10 Annie           4
# ... with 168 more rows