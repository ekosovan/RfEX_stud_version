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


# 2) How many men and women are represented in the dataset? 


# 3) What is the average age of women

# 4) What are the mean and standard deviation of age for those who earn more than 50K per year
# and those who earn less than 50K per year

# 5) Is it true that people who earn more than 50K have at least high school education? (education - 
#     Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters or Doctorate feature)

# 6) Display age statistics for each rate and each gender. Find the maximum age of men of 
# Amer-Indian-Eskimo race

# 7) Among whom is the proportion of those who earn a lot (>50K) is greater: married or single men?
#    Consider as married those who have marital-status starting with Married (Married-civ-spouse,
#    Married-spouse-absent or Married-AF-spouse)
# NOTE: this is regex question ;)

# 8) What is the maximum number of hours a person works per week? How many people work such a number
#    of hours, and what is the percentage of those who earn a lot (>50K) among them?

# 9) Count the average time of work for those who earn a little and a lot (salary) for 
#    each country (native-country). What will these be in Japan?


##############################################################################
# Problem 2 (30%)
##############################################################################


# This is a very simple problem, yet it is worth 30% of the PS grade
# Load titanic dataset. EXPLORE IT, if needed. Find the most common female name
# and create a sorted dataset with columns "female name" and "number of occurences", i.e.
# how many times the given name occured in the dataset.



