#############################################################################################
# INSTRUCTIONS
#############################################################################################
# 1) data can be downloaded from the link 
#    https://drive.google.com/file/d/1pmS-rPeUwE0MZfgGMiKjNq-koNlRiXg9/view?usp=sharing
# 2) project has to be done INDIVIDUALLY
# 3) submission of the problem set should be only this script with code added right below the
#    problem.
# 4) Please comment your code thoroughly (i.e. if there were any issues or it is difficult
#    to understand your code)




#############################################################################################
# TASK 1
#############################################################################################

# 1.1)
# load the data cpsmar12.dta  (find out about how to read .dta by googling). Do not forget to transform the
# result of read_*** into more common to R format
# Then select the first 100,000 observations

library(foreign)
library(magrittr)
library(tidyverse)

zipped = unzip("midterm.zip", list = TRUE)
zipped


households_full <- read.dta(unzip("midterm.zip", "cpsmar12.dta"))

households <- households_full[1:100000,]

df <- as.data.frame(households)


# To reduce memory consumption remove unnecessary files. In environment there should only be 
# a single dataframe

# 1.2)
# write function that would have dataframe and a vector
# as an input.
# The function should return a dataframe with two columns
# "indicator"  and "available_series" where the first column is just the 
# elements of the input vector, and the "available" is boolean
# vector with TRUE if indicator is contained in the dataframe
# call this dataframe available_series

indicators = c('ph_seq','ffpos', 'pppos','region','gereg', 'lfstat','a_lfsr','h_year','a_age',
              'race','prdtrace','hispanic','prdthsp','educ_cat','a_hge','lths','hs','rsnnottw' ,
              'so_col','college', 'grad' ,'semp_yn','a_sex','fwsval',
              'not_working','selfempl','wages' ,'hrsly','hrslt','female' ,'married',
              'divorced', 'children')

function_available_series <- function(indicator, data) {
  available <- indicator %in% colnames(data)
  columns <- cbind(indicator, available)
  as.data.frame(columns)
}

avai_series <- function_available_series(indicators, df)
available_series <- as.data.frame(avai_series)
available_series

# I prepare a vector of desired columns names to work with them in the following problems

des_columns <- available_series %>%
  filter(available == TRUE)
desired_columns <- as.vector(des_columns[[1]])
desired_columns

#1.3)
# Create a slice of the dataframe for those indicators for which column available is TRUE. 
# Call the result df_slice
# You can either do that by creating a function or without creating a function

function_slice_available <- function(data, columns) {
  data[, which(colnames(data) %in% columns)]
}

df_slice_notordered <- function_slice_available(df, desired_columns)

#the following line changes the order of columns in df_slice_notordered so that I can easily rename them in the task 3
df_slice <- df_slice_notordered[desired_columns]


#############################################################################################
# Task 2
#############################################################################################
# You're gonna continue working with the slice only. Feel free to rename it and to remove any unnecessary
# variables


#2.1) Name of the columns are not really informative.Rename columns using col_names vector
col_names = c("household_id","family_id","individual_id",'region','employment_status',
              'year','age','race','hispanic','self_employed','sex','family_income')

names(df_slice) <- col_names
summary(df_slice)


# 2.2)
# Create a function that has dataframe df as an input and returns you the number of households,
# number of families and number of individuals within the dataframe df

#using magrittr

mag_households <- df_slice %>%
  count(household_id) %>%
  nrow()

mag_families <- df_slice %>%
  group_by(household_id) %>%
  count(family_id) %>%
  nrow()
  
mag_individuals <- df_slice %>%
  group_by(household_id, family_id) %>%
  count(individual_id) %>%
  nrow()

mag_households
mag_families
mag_individuals
  
#not using pipelines - generally
unique_households <- count(df_slice, household_id)
households <- nrow(unique_households)
households_grouping <- group_by(df_slice, household_id)
unique_families <- count(households_grouping, family_id)
families <- nrow(unique_families)
families_grouping <- group_by(df_slice, household_id, family_id)
unique_individuals <- count(d, individual_id)
individuals <- nrow(unique_individuals)
households
families
individuals

#when writing it as a single function
function_number_items <- function(data) {
  unique_households <- count(data, household_id)
  households <- nrow(unique_households)
  households_grouping <- group_by(data, household_id)
  unique_families <- count(households_grouping, family_id)
  families <- nrow(unique_families)
  families_grouping <- group_by(data, household_id, family_id)
  unique_individuals <- count(families_grouping, individual_id)
  individuals <- nrow(unique_individuals)
  cat("In the dataframe, there are", households, "households,", families, "families and", individuals, "individuals")
}

function_number_items(df_slice)


# 2.3)
# Calculate the number of self_employed for males and females

df_slice %>%
  group_by(sex) %>%
  filter(sex == "Male") %>%
  count(self_employed == "Yes")

df_slice %>%
  group_by(sex) %>%
  filter(sex == "Female") %>%
  count(self_employed == "Yes")


# 2.4)
# Calculate the share of self_employed males and females

Males <- filter(df_slice, sex == "Male")
nMales <- nrow(Males)
Males_self_employed <- filter(Males, self_employed == "Yes")
nMales_self_employed <- nrow(Males_self_employed)
Males_share = nMales_self_employed / nMales

Females <- filter(df_slice, sex == "Female")
nFemales <- nrow(Females)
Females_self_employed <- filter(Females, self_employed == "Yes")
nFemales_self_employed <- nrow(Females_self_employed)
Females_share = nFemales_self_employed / nFemales

Males_share
Females_share


#############################################################################################
# Task 3
#############################################################################################


#Load athlete_events dataframe. Below is the decription of the dataframe 
zipped

athletes <- read.csv(unzip("midterm.zip", "athlete_events.csv"))
as.data.frame(athletes)

#ID - Unique number for each athlete
#Name - Athlete's name
#Sex - M or F
#Height - In centimeters
#Weight - In kilograms
#Team - Team name
#NOC - National Olympic Committee 3-letter code
#Games - Year and season
#Year - Integer
#Season - Summer or Winter
#City - Host city
#Sport - Sport
#Event - Event
#Medal - Gold, Silver, Bronze, or NA

# NOTE: In order to receive any points you need to program your answer (just writing "smth is equal 35%) will
# give earn you ZERO points.
# Try to use magrittr pipe operators in order to avoid creating extra useless variables.

# 3.1)
# How old were the youngest male and female participants of the 1992 Olympics?

summary(athletes)

# The following functions give us correct results as lists.
athletes %>%
  filter(Year == 1992, Sex == "M") %>%
  summarise(youngest_male_1992 = min(Age, na.rm = T), oldest_male_1992 = max(Age, na.rm = T))

athletes %>%
  filter(Year == 1992, Sex == "F") %>%
  summarise(youngest_female_1992 = min(Age, na.rm = T), oldest_female_1992 = max(Age, na.rm = T))

# Here, I am slightly modifying previous functions to print the results as whole sentences using cat function.
# I personally prefer seeing the results in lists but this is also an option.
males_1992 <- athletes %>%
  filter(Year == 1992, Sex == "M") %>%
  summarise(youngest_male_1992 = min(Age, na.rm = T), oldest_male_1992 = max(Age, na.rm = T))

cat("The youngest male athlete in 1992 was", males_1992[[1]], "and the oldest", males_1992[1,2])

females_1992 <- athletes %>%
  filter(Year == 1992, Sex == "F") %>%
  summarise(youngest_female_1992 = min(Age, na.rm = T), oldest_female_1992 = max(Age, na.rm = T))

cat("The youngest female athlete in 1992 was", females_1992[[1]], "and the oldest", females_1992[1,2])
  
# 3.2)
# What was the percentage of male basketball players amongh all the male participants of the 2012 Olympics?
# Round answer to the first decimal

# I am using unique IDs in the following function, not unique names, as these two numbers differ
# and I assume there are probably few participants of the same name but of course different ID

males_athletes_2012 = athletes %>%
  filter(Sex == "M") %>%
  filter(Year == 2012) %>%
  summarize(males_number = length(unique(ID)))

males_basketball_2012 = athletes %>%
  filter(Sex == "M") %>%
  filter(Year == 2012) %>%
  filter(Sport == "Basketball") %>%
  summarize(basketball_number = length(unique(ID)))

share <- (males_basketball_2012 / males_athletes_2012) * 100
rounded_share <- round(share[[1]], 1)

cat("The share of male basketball players among all male athletes in 2012 was", rounded_share[[1]], "%")

# 3.3)
# What are the mean and standard deviation of height for female tennis players who participated in the 2000
# Olympics. Round to the first decimal

athletes %>%
  filter(Sex == "F", Year == 2000, Sport == "Tennis") %>%
  summarise(mean_height = round(mean(Height, na.rm = T), 1), standard_deviation_height = round(sd(Height, na.rm = T), 1))


# 3.4)
# Find the heaviest athete among 2006 Olympics participants. What sport did he or she do?

heavy_sport <- athletes %>%
  filter(Year == 2006) %>%
  filter(Weight == max(Weight, na.rm = T)) %>%
  pull(Sport)

cat("The heaviest athlete in 2016 did", as.character(heavy_sport))

# 3.5)
# How many time did John Aalberg participate in the Olympics held in different years

JA_years <- athletes %>%
  group_by(Year) %>%
  filter(Name == "John Aalberg") %>%
  summarise(number_of_different_years = length(unique(Year))) %>%
  nrow()

cat("John Aalberg participated in the Olympics in", JA_years, "different years")

# 3.6)
# How many gold medals in tennis did the Switzerland team win at the 2008 Olympics?

Swiss_gold_2008 <- athletes %>%
  filter(Year == 2008, Team == "Switzerland", Medal == "Gold", Sport == "Tennis") %>%
  nrow()

cat("In 2008, Switzerland won", Swiss_gold_2008, "gold medals in tennis")

# 3.7)
# Is it true that there were Summer Olympics held in Atlanta? Is it true that there were Winter Olympics
# held in Squaw Valley

Atlanta_summer <- athletes %>%
  filter(City == "Atlanta", Season == "Summer") %>%
  nrow()

Determine_Atlanta <- Atlanta_summer > 0
cat("There were Summer Games in Atlanta.", Determine_Atlanta)

Squaw_Valley_winter <- athletes %>%
  filter(City == "Squaw Valley", Season == "Winter") %>%
  nrow()

Determine_Squaw_Valley <- Squaw_Valley_winter > 0
cat("There were Winter Games in Squaw Valley.", Determine_Squaw_Valley)

# 3.8)
# Is it true that Spain won fewer medals than Italy at the 2016 Olymptics?

Spain <- athletes %>%
  na.omit() %>%
  filter(Year == 2016, Team == "Spain") %>%
  nrow()

Italy <- athletes %>%
  na.omit() %>%
  filter(Year == 2016, Team == "Italy") %>%
  nrow()

Determine_SP_IT <- Spain < Italy
cat("Spain won fewer medals than Italy in 2016.", Determine_SP_IT)

# 3.9)
# What is the absolute difference between the number of unique sports at the 1986 Olympics and 2002 Olympics?

# There were no Games in 1986, therefore, the following function will give 0

Sports_1986 <- athletes %>%
  filter(Year == 1986) %>%
  nrow()

Sports_2002 <- athletes %>%
  filter(Year == 2002) %>%
  pull(Sport) %>%
  unique() %>%
  length()

difference = abs(Sports_1986 - Sports_2002)
cat("The absolute difference between the number of unique sports at 1986 and 2002 is", difference)