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


install.packages("tidyverse")#I guess that tidyverse has some packages already (dplyr, tidyr)
install.packages("haven")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

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

cpsmar12 <- read_dta("cpsmar12.dta")
cpsmar12<- data.frame(cpsmar12)
head(cpsmar12)
class(cpsmar12)#to see if it is a dataframe
new_data <- cpsmar12 %>%
  slice(1:100000)

# To reduce memory consumption remove unnecessary files. In environment there should only be 
# a single dataframe

rm(cpsmar12)

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

rm(available_series2) #getting rid of one dataframe to reduce memory

#1.3)
# Create a slice of the dataframe for those indicators for which column available is TRUE. 
# Call the result df_slice
# You can either do that by creating a function or without creating a function

df_slice <- available_series %>%
  filter(available_series$available,TRUE) %>%
  pull(1) #pulling only names of the columns to get a nice vector

df_slice <- as.vector(df_slice) #transforming factors into a vector with characters

#using dplyr
selected_data <- new_data %>%
  select(df_slice) 

#practising by using a function (however too complicated)
select_data <- function(df,v){
  value=data.frame(nrow=dim(df)[1])#generating random dataset with one column and the same number of rows as df
  for (i in 1:length(names(df))){
    if (names(df[i]) %in% v == TRUE){
    value <- cbind(value,df[i])  
    }
    else{
    }
  }
  return(value[,-1])#removing a random generated column
}

alternative <- select_data(new_data,df_slice)

rm(alternative)#removing alternative for the transparency

#############################################################################################
# Task 2
#############################################################################################
# You're gonna continue working with the slice only. Feel free to rename it and to remove any unnecessary
# variables


#2.1) Name of the columns are not really informative. Rename columns using col_names vector
col_names = c("household_id","family_id","individual_id",'region','employment_status',
              'year','age','race','hispanic','self_employed','sex','family_income')

names(selected_data) <- col_names

# 2.2)
# Create a function that has dataframe df as an input and returns you the number of households,
# number of families and number of individuals within the dataframe df

#approach using dplyr package
fun2 <- function(df){
n_family <- selected_data %>%
  count(family_id)%>%
  summarise(unique_family=n_distinct(family_id,na.rm=TRUE))

n_household <- selected_data %>%
  count(household_id)%>%
  summarise(unique_households=n_distinct(household_id,na.rm=TRUE))

n_individual <- selected_data %>%
  count(individual_id)%>%
  summarise(unique_individuals=n_distinct(individual_id,na.rm=TRUE))

return(c(n_household,n_family,n_individual))
}


fun2(selected_data)

#There are 37451 housholds, 12 families and 15 individuals. 


#another classical approach - I also put output in dataframe for better working in future
fun3 <- function(df){
n_household <- length(unique(selected_data$household_id))
n_family <- length(unique(selected_data$family_id))
n_individual <- length(unique(selected_data$individual_id))
return(data.frame(type=c("n_households", "n_family", "n_individual"),number=c(n_household,n_family,n_individual)))
}

fun3(selected_data)

# 2.3)
# Calculate the number of self_employed for males and females

str(selected_data$self_employed) #thanks to this I found what 0 1 2 means --> 1 means yes, other no
selected_data %>%
  group_by(sex)%>%
  count(self_employed, name="number") #I prefer this instead of filtering out only self-employed as I can see the whole picture

#more aggregated results would be using the filter
selected_data %>%
  group_by(sex)%>%
  filter(self_employed==1)%>%
  count(self_employed, name="number")
  
#There are 2131 males and 1623 females who are self employed.

# 2.4)
# Calculate the share of self_employed males and females

#here is the share comparing self_emploed males and females among all self_employed
selected_data %>%
  group_by(sex)%>%
  count(self_employed, name="number")%>%
  filter(self_employed==1)%>%
  ungroup()%>%
  mutate(share=number/sum(number)*100)

#The share of self_employed males is 56.8% and females 43.2%.

#here is the share comparing self_employed male and female in male/female group
#I keep all shares to see a transparent results and to be able to compare them
selected_data %>%
  group_by(sex)%>%
  count(self_employed, name="number")%>%
  mutate(share=number/sum(number)*100)

#The share of self-employed male is 4.41% among men and female 3.14% among females. 

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

rm(list=ls()) #cleaning environment to make it more transparent for you

athlete_events <- read_csv("athlete_events.csv")
glimpse(athlete_events) #I use this to quickly check on the structure of the data
head(athlete_events) #This I use to get the idea about the info stored there

# 3.1)
# How old were the youngest male and female participants of the 1992 Olympics?
athlete_events %>%
  filter(Year==1992)%>%
  group_by(Sex)%>%
  summarize(youngest=min(Age,na.rm=TRUE))

#The youngest male was 11 years old and Female 12 years old.

# 3.2)
# What was the percentage of male basketball players amongh all the male participants of the 2012 Olympics?
# Round answer to the first decimal

#here I also keep the other to get the idea and compare - however, using filter on Basketball will do the thing
athlete_events %>%
  filter(Year==2012,Sex=="M")%>%
  count(Sport)%>%
  ungroup()%>%
  mutate(share=round(n/sum(n)*100,1))#%>%
  #filter(Sport=="Basketball")

#The percentage of male basketball players among all the male participants of the 2012 Olympics was 2.0%.


# 3.3)
# What are the mean and standard deviation of height for female tennis players who participated in the 2000
# Olympics. Round to the first decimal

athlete_events %>%
  filter(Year==2000,Sex=="F",Sport=="Tennis")%>%
  summarize(mean=round(mean(Height,na.rm=TRUE),1),stddev=round(sd(Height,na.rm=TRUE),1))

#The mean was 172.0 and standard deviation was 6.5.



# 3.4)
# Find the heaviest athlete among 2006 Olympics participants. What sport did he or she do?

athlete_events %>%
  filter(Year==2006)%>%
  filter(Weight==max(Weight,na.rm=TRUE))

#The heaviest athlete among 2006 Olympics participants weighted 127 units and he did skeleton. 

# 3.5)
# How many time did John Aalberg participate in the Olympics held in different years

athlete_events %>%
  filter(Name=="John Aalberg")%>%
  count(Year)#%>%
  #summarise(unique_years=n_distinct(Year,na.rm=TRUE))
#as there are only two years, I consider this result much nicer however, it can be expanded by summarise in comment

#John Aalberg participated in two different Olympics.

# 3.6)
# How many gold medals in tennis did the Switzerland team win at the 2008 Olympics?

athlete_events %>%
  filter(Team=="Switzerland",Year==2008,Sport=="Tennis")%>%
  na.omit(Medal)%>%
  count(Medal)

#Switzerland team won 2 gold medals at the 2008 Olympics.

# 3.7)
# Is it true that there were Summer Olympics held in Atlanta? 

athlete_events %>%
  filter(Season=="Summer",City=="Atlanta")#from the filter,we can clearly see that there was SO in Atlanta

#Yes, there was Summer Oplympics in Atlanta.

#other approach
any(athlete_events$Season=="Summer" & athlete_events$City=="Atlanta")

#Is it true that there were Winter Olympics held in Squaw Valley?

athlete_events %>% #same with filter approach here
  filter(Season=="Winter",City=="Squaw Valley")

#Yes, there was Winter Oplympics in Squaw Valley.

#other approach
any(athlete_events$Season=="Winter" & athlete_events$City=="Squaw Valley")

# 3.8)
# Is it true that Spain won fewer medals than Italy at the 2016 Olymptics?

athlete_events %>% #this gives us nice overview of all medals won by Italy/Spain as well as NA data
  filter(Year==2016, Team==c("Spain","Italy"))%>%
  group_by(Team)%>%
  count(Medal)#I think it is good to keep there NA data in this case to see how many actualy attempts there were or how complete the data are (in case NA means value missing)

athlete_events %>% #this gives us an overview od all won medals by Italy/Spain
  filter(Year==2016, Team==c("Spain","Italy"))%>%
  na.omit(Medal)%>%
  group_by(Team)%>%
  tally(name = "number_of_medals")

#Yes, Italy won 12 more medals than Spain in 2016.

# 3.9)
# What is the absolute difference between the number of unique sports at the 1986 Olympics and 2002 Olympics?

athlete_events %>%
  summarise(min(Year, na.rm=TRUE)) #there is no year 1986 - only a year 1896 - probably this was the year you mentioned?

athlete_events %>% #overview for two countries, code in comment will provide absolute difference
  filter(Year==c("2002","1896"))%>%
  group_by(Year)%>%
  summarise(unique_sports=n_distinct(Sport,na.rm=TRUE))#%>%
  #summarise(difference=diff(unique_sports))

#The absolute difference between unique sports at the 1896 and 2002 is 6.
