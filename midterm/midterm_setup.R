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



#1.3)
# Create a slice of the dataframe for those indicators for which column available is TRUE. 
# Call the result df_slice
# You can either do that by creating a function or without creating a function


#############################################################################################
# Task 2
#############################################################################################
# You're gonna continue working with the slice only. Feel free to rename it and to remove any unnecessary
# variables


#2.1) Name of the columns are not really informative.Rename columns using col_names vector
col_names = c("household_id","family_id","individual_id",'region','employment_status',
              'year','age','race','hispanic','self_employed','sex','family_income')

# 2.2)
# Create a function that has dataframe df as an input and returns you the number of households,
# number of families and number of individuals within the dataframe df



# 2.3)
# Calculate the number of self_employed for males and females


# 2.4)
# Calculate the share of self_employed males and females


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

# 3.1)
# How old were the youngest male and female participants of the 1992 Olympics?

# 3.2)
# What was the percentage of male basketball players amongh all the male participants of the 2012 Olympics?
# Round answer to the first decimal


# 3.3)
# What are the mean and standard deviation of height for female tennis players who participated in the 2000
# Olympics. Round to the first decimal


# 3.4)
# Find the heaviest athete among 2006 Olympics participants. What sport did he or she do?


# 3.5)
# How many time did John Aalberg participate in the Olympics held in different years

# 3.6)
# How many gold medals in tennis did the Switzerland team win at the 2008 Olympics?

# 3.7)
# Is it true that there were Summer Olympics held in Atlanta? Is it true that there were Winter Olympics
# held in Squaw Valley

# 3.8)
# Is it true that Spain won fewer medals than Italy at the 2016 Olymptics?


# 3.9)
# What is the absolute difference between the number of unique sports at the 1896 Olympics and 2002 Olympics?