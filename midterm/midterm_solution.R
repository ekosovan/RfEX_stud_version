install.packages('haven')
gc()

library('haven')
library('tidyverse')
library('magrittr')
#############################################################################################
# TASK 1
#############################################################################################

# 1.1)
# load the data in .dta format (find out about how to read it by googling). Do not forget to transform the
# result of read_*** into more common to R format
# Then select the first 100,000 observations

raw_data = read_dta('cpsmar12.dta')
df = as_factor(raw_data)[1:100000,]

# To reduce memory consumption remove unnecessary files. In environment there should only be 
# a single dataframe

rm('raw_data')

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


check_availability = function(df,indicators){
  available = indicators %in% colnames(df)
  return(data.frame(indicators, available))
}


available_series = check_availability(df, indicators)
available_series

#1.3)
# Create a slice of the dataframe for those indicators for which column available is TRUE. 
# Call the result df_slice
# You can either do that by creating a function or without creating a function



available_indicators = available_series %>% filter(available == T) %>% pull(indicators) %>% as.character()
df_slice = df %>% select(available_indicators)
head(df_slice)
rm('available_indicators')

#############################################################################################
# Task 2
#############################################################################################
# You're gonna continue working with the slice only. Feel free to rename it and to remove any unnecessary
# variables

df = df_slice
rm('df_slice', 'available_indicators')

head(df,10)

#2.1) Name of the columns are not really informative.Rename columns using col_names vector

col_names = c("household_id","family_id","individual_id",'region','employment_status',
              'year','age','race','hispanic','self_employed','sex','family_income')
colnames(df) = col_names  

head(df)  

# 2.2)
# Create a function that has dataframe df as an input and returns you the number of households,
# number of families and number of individuals within the dataframe df

number_of_unique_obs <- function(data) {
  households = data %>% count(household_id) %>% nrow()
  families =  data %>% group_by(household_id) %>% count(family_id) %>% nrow()
  individuals =data %>% group_by(household_id,family_id) %>% count(.,individual_id) %>% nrow()
  cat("In the dataframe, there are", households, "households,", families, "families and", individuals, "individuals")
}

number_of_unique_obs(df)

# 2.3)
# Calculate the number of self_employed for males and females

df %>% select(household_id, family_id,individual_id,sex, self_employed) %>% 
  group_by(sex) %>% summarize(businessmen= sum(self_employed =="Yes"))

# 2.4)
# Calculate the share of self_employed males and females

# as a share of all self-employed
df %>%
  group_by(sex)%>%
  count(self_employed, name="number")%>%
  filter(self_employed=='Yes')%>%
  ungroup()%>%
  mutate(share=number/sum(number)*100)

# as a share of all sample
df %>%
  group_by(sex)%>%
  count(self_employed, name="number")%>%
  mutate(share=number/sum(number)*100)



#############################################################################################
# Task 3
#############################################################################################
rm(list=ls())

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

# 3.1)
# How old were the youngest male and female participants of the 1992 Olympics?
athlete_events <- read_csv("athlete_events.csv")
glimpse(athlete_events)

# 3.2)
# What was the percentage of male basketball players amongh all the male participants of the 2012 Olympics?
# Round answer to the first decimal
athlete_events %>%
  filter(Year==1992)%>%
  group_by(Sex)%>%
  summarize(youngest=min(Age,na.rm=T))


# 3.3)
# What are the mean and standard deviation of height for female tennis players who participated in the 2000
# Olympics. Round to the first decimal

athlete_events %>%
  filter(Year==2000,Sex=="F",Sport=="Tennis")%>%
  summarize(mean=round(mean(Height,na.rm=T),1),stddev=round(sd(Height,na.rm=T),1))

# 3.4)
# Find the heaviest athete among 2006 Olympics participants. What sport did he or she do?

athlete_events %>%
  filter(Year==2006)%>%
  filter(Weight==max(Weight,na.rm=T)) %>% 
  print() %>% 
  pull(Sport)

# 3.5)
# How many time did John Aalberg participate in the Olympics held in different years


athlete_events %>%
  filter(Name=="John Aalberg")%>%
  count(Year)%>%
  summarise(unique_years=n_distinct(Year))

# 3.6)
# How many gold medals in tennis did the Switzerland team win at the 2008 Olympics?

athlete_events %>%
  filter(Team=="Switzerland",Year==2008,Sport=="Tennis")%>%
  na.omit(Medal)%>%
  count(Medal)

# 3.7)
# Is it true that there were Summer Olympics held in Atlanta? Is it true that there were Winter Olympics
# held in Squaw Valley


any(athlete_events$Season=="Summer" & athlete_events$City=="Atlanta")
any(athlete_events$Season=="Winter" & athlete_events$City=="Squaw Valley")

# 3.8)
# Is it true that Spain won fewer medals than Italy at the 2016 Olymptics?

athlete_events %>% #this gives us an overview od all won medals by Italy/Spain
  filter(Year==2016, Team==c("Spain","Italy"))%>%
  na.omit(Medal)%>%
  group_by(Team)%>%
  tally(name = "number_of_medals")

# 3.9)
# What is the absolute difference between the number of unique sports at the 1986 Olympics and 2002 Olympics?

# Sorry for messing this one up. This was supposed to be 1896 not 1986

athlete_events %>% 
  filter(Year==c("2002","1896"))%>%
  group_by(Year)%>%
  summarise(unique_sports=n_distinct(Sport,na.rm=T))%>%
  summarise(difference=diff(unique_sports))
