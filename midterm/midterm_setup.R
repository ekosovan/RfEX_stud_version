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


install.packages("tidyverse")
library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)


#############################################################################################
# TASK 1
#############################################################################################

# 1.1)
# load the data cpsmar12.dta  (find out about how to read .dta by googling). Do not forget to transform the
# result of read_*** into more common to R format
# Then select the first 100,000 observations

cpsmar12 <- read_dta("midterm/cpsmar12.dta")
data<- data.frame(cpsmar12)
head(data)
new_data <- data %>%
  slice(1:1000)

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

#checking each element using for loop and storing it into new vector
availability <- function(df,v){
  value=c()
  for (i in 1:length(v)){
    value <- append(value,indicators[i] %in% names(df))  
  }
  available_series <- data.frame(indicator=indicators,available=value)
  return(available_series)
}

#more smooth vay of checking and creating whole vector and store it directly in data frame
availability2 <- function(df,v){
  value=indicators %in% names(df) 
  available_series <- data.frame(indicator=indicators,available=value)
  return(available_series)   
}

available_series <- availability(new_data,indicators)
available_series2 <-availability2(new_data,indicators)

#1.3)
# Create a slice of the dataframe for those indicators for which column available is TRUE. 
# Call the result df_slice
# You can either do that by creating a function or without creating a function
# But note full points will be 

df_slice <- available_series %>%
  filter(available_series$available,TRUE) %>%
  pull(1)

df_slice <- as.vector(df_slice)

selected_data <- new_data %>%
  select(df_slice)

map.df(new_data,select %in% df_slice)

#############################################################################################
# Task 2
#############################################################################################
# You're gonna continue working with the slice only. Feel free to rename it and to remove any unnecessary
# variables


#2.1) Name of the columns are not really informative. Rename columns using col_names vector


# 2.2)
# Create a function that has dataframe df as an input and returns you the number of households,
# number of families and number of individuals within the dataframe df

fun1 <- function(df){
  value=indicators %in% names(df) 
  available_series <- data.frame(indicator=indicators,available=value)
  return(available_series)   
}


# 2.3)
# Calculate the number of self_employed for males and females

str(sliced_data$semp_yn) #thanks to this I found what 0 1 2 means --> 1 means yes, other no
sliced_data %>%
  group_by(a_sex)%>%
  count(semp_yn)

#more aggregated results would be


# 2.4)
# Calculate the share of self_employed males and females

self_employed <- cbind(self_employed,share=self_employed$number/sum(sliced_data$semp_yn))

#elegant solution
sliced_data %>%
  group_by(a_sex)%>%
  count(semp_yn, name="number")%>%
  filter(semp_yn==1)%>%
  ungroup()%>%
  mutate(share=number/sum(number)*100)



#############################################################################################
# Task 3
#############################################################################################


#Load athlete_events dataframe. Below is the decription of the dataframe 
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

athlete_events <- read_csv("midterm/athlete_events.csv")
glimpse(athlete_events)
head(athlete_events)

# 3.1)
# How old were the youngest male and female participants of the 1992 Olympics?
athlete_events %>%
  filter(Year==1992)%>%
  group_by(Sex)%>%
  summarize(youngest=min(Age,na.rm=TRUE))

# 3.2)
# What was the percentage of male basketball players amongh all the male participants of the 2012 Olympics?
# Round answer to the first decimal

athlete_events %>%
  filter(Year==1992,Sex=="M")%>%
  count(Sport)%>%
  mutate(share=round(n/sum(n)*100,1))


# 3.3)
# What are the mean and standard deviation of height for female tennis players who participated in the 2000
# Olympics. Round to the first decimal

athlete_events %>%
  filter(Year==2000,Sex=="F")%>%
  summarize(mean=round(mean(Height,na.rm=TRUE),1),stddev=round(sd(Height,na.rm=TRUE),1))

# 3.4)
# Find the heaviest athete among 2006 Olympics participants. What sport did he or she do?

athlete_events %>%
  filter(Year==2006)%>%
  filter(Weight==max(Weight,na.rm=TRUE))

#another solution

athlete_events[which.max(athlete_events$Weight & athlete_events$Year==2006),]


# 3.5)
# How many time did John Aalbers participate in the Olympics held in different years

athlete_events %>%
  filter(Name=="John Aalberg")%>%
  group_by(Year)%>%
  count(Year)

# 3.6)
# How many gold medals in tennis did the Switzerland team win at the 2008 Olympics?

athlete_events %>%
  filter(Team=="Switzerland",Year==2008)%>%
  count(Medal)

# 3.7)
# Is it true that there were Summer Olympics held in Atlanta? Is it true that there were Winter Olympics
# held in Squaw Valley

athlete_events %>%
  filter(Season=="Summer",City=="Atlanta")

#other approach
any(athlete_events$Season=="Summer" & athlete_events$City=="Atlanta")

athlete_events %>%
  filter(Season=="Winter",City=="Squaw Valley")

#other approach
any(athlete_events$Season=="Winter" & athlete_events$City=="Squaw Valley")

# 3.8)
# Is it true that Spain won fewer medals than Italy at the 2016 Olymptics?

athlete_events %>%
  filter(Year==2016, Team==c("Spain","Italy"))%>%
  group_by(Team)%>%
  count(Medal)

athlete_events %>%
  filter(Year==2016, Team==c("Spain","Italy"))%>%
  na.omit(Medal)%>%
  group_by(Team)%>%
  tally(name = "number_of_medals")


# 3.9)
# What is the absolute difference between the number of unique sports at the 1986 Olympics and 2002 Olympics?
athlete_events %>%
  filter(Year==2002)%>%
  summarise(unique_sports=n_distinct(Sport))


