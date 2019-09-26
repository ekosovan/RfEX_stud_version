# IDE overview ------------------------------------------------------------
# 1.) R & RStudio version
# 2.) Overview: Source, console, History, Environment, Files, Packages, Help
# 3.) Setup: Color, Positions, Best practise 
# 4.) Packages
# 5.) Git setup
# 

######################################################################################################################################
# Coding basics
######################################################################################################################################

######################################################################################################################################
#You may use R as calculator

# Addition
2+3

# Sibtraction
10-5

# Multiplication
12*9

# Division
10/5

# Exponentiation
2^4

# Modulo

28%%6

######################################################################################################################################
# Variable asignment
# You can create new variables by using assignment symbol <- or =
a <- 5
b = 4
c = a + b

# Look: new variables appear in the Environment tab

mae_students = 10
phd_students = 33

all_students = mae_students+phd_students
all_students

phd_students  = 'thirty three'
all_students = mae_students + phd_students

# Note: running the last line raises an error. This is because phd_students is now a string and R does not know
# how to add a number and a string

# Note 2 :  When naming variables, remember that names should be descriptive
# typicall convention is to use lowercase letters and _ as a separator

######################################################################################################################################
# Data types in R

my_number = 6

my_string = 'twelve'

my_string_2 = '12'

my_logical = TRUE

my_logical_2 = T

# To check data type you may use function class()

class(my_logical)

######################################################################################################################################
# Vectors
######################################################################################################################################


num_vector = c(5,7,8)

char_vector = c('a', 'b', 'd')

bool_vector = c(T,F,T)

mixes_vector = c(T, 2, 'c')
  

lottery_payoffs = c(12,-4,-7, 3)  
poker_payoffs = c(5,6,-5,12)

names(lottery_payoffs) = c('win', "loss", 'loss', 'win') 
names(poker_payoffs) = c('win', 'win', 'loss', 'win')

lottery_payoffs  
poker_payoffs

# Now you can refer to elements of the vector by their name

poker_payoffs['loss']


lottery_payoffs['win']

# Note 'win' occurs two times in lottery_payoffs, and as an output we get the first occurence

# We can also use numeric indexing

lottery_payoffs[2:3]

poker_payoffs[1]

######################################################################################################################################
# Vector operations

a = c(2,3,5)
b = c(5,-7,-9)

# Note a and b in the enrironment has changed to vectors and previous values were deleted

# Addition
c = a+b

# Subtraction
d = a-b

# Sum of all elements
sum_b = sum(b)
sum_a = sum(a)

# Mean

mean(a)


######################################################################################################################################
# Selection

# Check elementwise if the condition holds

selection = a > 0
selection

selected_elements = a[selection]
selected_elements

# Alternatively you can do it in a single expression

selected_elements_2 = a[a>0]

######################################################################################################################################
# Matrices
######################################################################################################################################

my_matrix = matrix(1:12, byrow = TRUE, nrow  = 4)

colnames(my_matrix) = c('A', 'B', 'MAE')
rownames(my_matrix) = c('K', 'G', 'B','MAE')

my_matrix

dim(my_matrix)

######################################################################################################################################
# Factors
######################################################################################################################################

student_status = c('A', 'R', 'A', 'A', 'R')
factor_student_status = factor(student_status)

levels(factor_student_status) = c('Admitted', 'Rejected')

######################################################################################################################################
# DataFrames
######################################################################################################################################

install.packages('gapminder')
library('gapminder')

df = gapminder_unfiltered

head(df, 10)

str(df)

df[1,1] # (1,1) element
df[1,2] # (1,2) element

df[,1] # 1st col

df[1,] # 1st row

df$gdpPercap[1:5] # first 5 values of dfpPercap columns

subset(df,subset = (gdpPercap>100000) )  # take only those rows, where value of gdpPercap >100000


positions = order(df$gdpPercap) 
df[positions,] # reorder by the value of gdpPercap from smaller to bigger values

######################################################################################################################################
# List
######################################################################################################################################

# In R (and Python) list is a universal array that can contain multiple types of data

a
b
mixes_vector

my_list = list(a,b,mixes_vector) # create a list

my_list[1]  # first element of the list
my_list[3]  # third element of the list

names(my_list) = c('vector a', 'vector b', 'some weird stuff')

my_list

my_list["vector a"]


my_list[["vector a"]][1] # select the first element of "vector a" in the list 'my_list'

my_list[[1]][1] # another option to do the same thing







