##############################################################################
# Final Project
##############################################################################

##############################################################################
# Instructions
##############################################################################
# 1) You should submit the .R script with the solution
# 2) Make sure to comment wherever it is needed
# 3) Every student should submit his\her own copy
# 4) Pay attention to the question (if question asks for regex and you solve it differently - no points are awarded)
# 5) You cannot use attach(dataframe)

##############################################################################
# Part 1: Welcome to Credit Risk Analysis
##############################################################################

# You will be working on bankloans.csv. The data contains the credit details about credit borrowers: 
# Data Description:
  
# age - Age of Customer
# ed - Eductation level of customer
# employ: Tenure with current employer (in years)
# address: Number of years in same address
# income: Customer Income
# debtinc: Debt to income ratio
# creddebt: Credit to Debt ratio
# othdebt: Other debts
# default: Customer defaulted in the past (1= defaulted, 0=Never defaulted)

library(tidyverse)
library(magrittr)
library(psych) # will be usable in the first function (some of you used a part of it)

bankloans = read_csv('bankloans.csv')[,1:9] # im going to illustate the functions on the dataframe


# 1) Create a function 'var_summary' that has column (or vector) as an input and returns you a table with 
#     number of observations, number of missing values, sum of all values, mean, standard deviation,
#     variance, minimum, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95 and 0.99 quantiles and maximum
#     i.e. extended descriptive statistics

var_summary = function(df){
  statistics = describe(df, quant = c( 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99)) #calculates summary statistics
                                                                                            # + quantiles
  na_count <-sapply(df, function(y) sum(length(which(is.na(y))))) #calculates number of missing observations
  cbind(statistics, na_count) #combines the two
}

# Note the function can be applied to both dataframe and column
var_summary(bankloans)


# 2) Create a function 'create_dummies' that has two inputs - a dataframe and column_name
#   The function has to create dummy variables for categorical data stored in column_name, i.e.
#   suppose you have a single column  c('a','b','c','a') the return of the function has to produce 4x3 matrix with
#   a b c
#   1 0 0
#   0 1 0
#   0 0 1
#   1 0 0
#   The output of the function should be an input dataframe with dummies matrix added as new columns

# as an illustration we're gonna use 'ed' columns of bankloans data with 5 different levels

install.packages('dummies')
library(dummies)

dummy( bankloans$ed)

# or in the form of function
create_dummies = function(column){
  dummies::dummy(column)
}

create_dummies(bankloans$ed)

# I know, it is too easy, but I have never forbidden to use existing packages :)

# 3) Create a function 'outlier_capping' that would have vector or column as inputs. The function then 
#    replaces all right tail outliers (very huge numbers) with 0.99 quantile and all 
#    left tail outliers (big negative numbers) with 0.01 quantile of the column.
#    The function should output the same column\vector with processed outliers    

outlier_capping <- function(column){
  k <- quantile(column, probs = c(0.01, 0.99))
  column[which.max(column>k[2])] = k[2]
  column[which.min(column<k[1])] = k[1]
  return(column)
}
head(outlier_capping(bankloans$income),20)

# 4) Create a function 'process_missing' that would have a column\vector as an input. The function then
#    replaces all missing values with the mean and returns the processed column\vector of data.

process_missing <- function(column){
  mean = mean(column,na.rm=TRUE)
  column %<>%replace_na(mean)
  column
}

vector = c(1,2,5,656,NA, NA, 124, 123, NA)
process_missing(vector)

# 5) Apply the 'var_summary' function to each column of the data. (Use purrr maps or lapply or sapply - whichever you prefer). Any missing data?

var_summary(bankloans)

# 6) If everything is done correctly, there should be 150 NA's in 'default' column. Separate the dataframe into two
#    bankloans_existing and bankloans_new based on whether default value is present or missing


bankloans_existing = bankloans%>%filter(!is.na(default))
bankloans_new = bankloans%>%filter(is.na(default))

# 7) Apply outlier_capping and process_missing on every column of bankloans_existing

bankloans_existing = lapply(bankloans_existing,outlier_capping)
bankloans_existing = lapply(bankloans_existing,process_missing)
bankloans_existing <- as.data.frame(bankloans_existing)

# 8) Calculate the correlation matrix with correlations represented by color (google heatmap, there is really simple solution using on
#    of the packages mentioned in the course)

GGally::ggcorr(bankloans_existing)

# 9) Create a 2x4 matrix of boxplots where x = default, y is all other columns (all 8 plots should be on one picture)

bankloans_existing_new = bankloans_existing  %>%
                        gather(key = series, value =value,-default )



ggplot(bankloans_existing_new,aes(x=factor(default),y=value))+
      facet_wrap(~series,ncol = 2,scales='free')+
      geom_boxplot()

# 10) Create histogram of employ for those who defaulted (default = 1) and not (default  = 0) on the same graph (use color to distinguish 
#     defaulted and nondefaulted). Make histogram a bit transparent by setting alpha to 0.5 (hope you remember how to do it)


ggplot(bankloans_existing, aes(x = employ,color=factor(default))) +
  geom_histogram(alpha  = 0.5)


##############################################################################
# Part 2: ITS LOGIT TIME
##############################################################################

# 1) Estimate a logistic regression (google glm) of default on all other variables (use bankloans_existing data).
#    To do that you need to run the following commands

model  = glm(default ~.,                     # formula
             data  = bankloans_existing,     # data
             family = binomial(link = logit))# specification for the family of generalize linear model (by default within binomial family link is logit)



#     For references on the model please visit https://towardsdatascience.com/introduction-to-logistic-regression-66248243c148
#     Effectively, the fitted values are the estimated probabilities given person defaults or not, i.e. look at the observation
#     age  ed    employ address income    debtinc      creddebt       othdebt     default  model$fitted.values
#     41   3       17    12       176       9.3        11.35939      5.008608       1              0.7344775
#     The person with this properties has defaulted, and our model suggests that for him the probability to default was 73% (last column)


# 2) Create a confusion matrix for threshold 0.5, by running the following line.
#    For more info on confusion matrix https://towardsdatascience.com/taking-the-confusion-out-of-confusion-matrices-c1ce054b3d3e
#    Note, confusion matrixes are 2x2 matrices with elements
#    True positive (TP) | False Positive (FP)
#    False negative (FN)| True  Negative (TN)

table(bankloans_existing$default, model$fitted.values  > 0.5)

#    It is still not clear what should be a threshold default probability, i.e. probability of default we think is too high to lend 
#    money to a person. How to choose it? Well, there are multiple ways to do it.

# 3) Accuracy is one of the way it is calculating using the formula
#    accuracy =  (TP+TN)/(TP+TN+FP+FN)
#    it is effectively a fraction of all correctly specified 
#    Create a function that has inputs  default_column  and the predicted default probabilities (they have to have the same lenght!)
#    The function has to estimate accuracy for a grid of thresholds starting from 0.05 to 0.95 with increment 0.05, i.e.
#    0.05, 0.10, 0.15, ...., 0.85, 0.90, 0.95
#    The function should output the optimal threshold value, i.e. value for the threshold for which accuracy is maximized!

# I do it in two steps: first, I write a function that calculates a accuracy for a given threshold (just a number)
accuracy_score = function(model,column, threshold){
  conf_mat = table(bankloans_existing$default, model$fitted.values  > threshold)
  accuracy = (conf_mat[1,1]+conf_mat[2,2])/(sum(conf_mat))
  return(accuracy)
}
# then I apply map for a range of values
accuracy_vector = function(model,column){
  threshold_vector = seq(from = 0.05,to = 0.95, by = 0.05)
  score = sapply(threshold_vector,function(x){accuracy_score(model, column,x)})
  rbind(sequency, score)
}

# The rest is to compute max
accuracy_scores = accuracy_vector(model, bankloans_existing$default)
accuracy_scores

# 4) Another (and even more popular choice) is Specifity - Sesitivity score. Optimal threshold is chosen as one for which
#    the two the absolute difference abs(Specifity - Sensitivity) is closest to zero
#    Specifity  = TN/(TN+FP)
#    Sensitivity  = TP/(TP+FN)
#    Create a function that would calculate Specifity and Sensitivity for a the same grid as in part 3. The function should output
#    the threshold value for which the absolute difference is minimized

# Sensitivity can be handled in the exact same way


# 5) Are the estimated thresholds different between the two models?

# of course, they are not, mathematically expressions are different


# 6) Predict the default probabilities for the bankloans_missing ( model.predict). Obtain the probabilities of default. 
#    Convert them to booleans (1 or 0) based on either threshold estimated above. 

prediction = predict(model, bankloans_new, type = 'response')

default_or_no = ifelse(prediction>0.8, 1, 0)



# 7) Compute summary statistics for the obtained predictions and for the defaults column of bankloans_existing. Compute the fractions of
#    defaults (you may use one of the functions you programmed before). Do the results differ a lot?


# just apply var_summary function

# 8) Replace NA's in default columns of bankloans_missing with the predicted values. Append bankloans_missing with bankloans_existing

# trivial    

# 9) Save data as bankloans.xlsx with 3 sheets, on the first sheet there should be a dataframe you obtained in part 8 above, sheet 2 
#    must contain bankloans_existing dataframe, and sheet 3 must contain the transposed version of bankloans_missing dataframe

# totally googlable



