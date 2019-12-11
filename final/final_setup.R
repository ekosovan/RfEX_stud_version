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

# 1) Create a function 'var_summary' that has column (or vector) as an input and returns you a table with 
#     number of observations, number of missing values, sum of all values, mean, standard deviation,
#     variance, minimum, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95 and 0.99 quantiles and maximum
#     i.e. extended descriptive statistics


# 2) Create a function 'create_dummies' that has two inputs - a dataframe and column_name
#   The function has to create dummy variables for categorical data stored in column_name, i.e.
#   suppose you have a single column  c('a','b','c','a') the return of the function has to produce 4x3 matrix with
#   a b c
#   1 0 0
#   0 1 0
#   0 0 1
#   1 0 0
#   The output of the function should be an input dataframe with dummies matrix added as new columns


# 3) Create a function 'outlier_capping' that would have vector or column as inputs. The function then 
#    replaces all right tail outliers (very huge numbers) with 0.99 quantile and all 
#    left tail outliers (big negative numbers) with 0.01 quantile of the column.
#    The function should output the same column\vector with processed outliers    

# 4) Create a function 'process_missing' that would have a column\vector as an input. The function then
#    replaces all missing values with the mean and returns the processed column\vector of data.

# 5) Apply the 'var_summary' function to each column of the data. (Use purrr maps or lapply or sapply - whichever you prefer). Any missing data?

# 6) If everything is done correctly, there should be 150 NA's in 'default' column. Separate the dataframe into two
#    bankloans_existing and bankloans_new based on whether default value is present or missing

# 7) Apply outlier_capping and process_missing on every column of bankloans_existing


# 8) Calculate the correlation matrix with correlations represented by color (google heatmap, there is really simple solution using on
#    of the packages mentioned in the course)


# 9) Create a 2x4 matrix of boxplots where x = default, y is all other columns (all 8 plots should be on one picture)


# 10) Create histogram of employ for those who defaulted (default = 1) and not (default  = 0) on the same graph (use color to distinguish 
#     defaulted and nondefaulted). Make histogram a bit transparent by setting alpha to 0.5 (hope you remember how to do it)



##############################################################################
# Part 2: ITS LOGIT TIME
##############################################################################

# 1) Estimate a logistic regression (google glm) of default on all other variables (use bankloans_existing data).
#    To do that you need to run the following commands


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


# 4) Another (and even more popular choice) is Specifity - Sesitivity score. Optimal threshold is chosen as one for which
#    the two the absolute difference abs(Specifity - Sensitivity) is closest to zero
#    Specifity  = TN/(TN+FP)
#    Sensitivity  = TP/(TP+FN)
#    Create a function that would calculate Specifity and Sensitivity for a the same grid as in part 3. The function should output
#    the threshold value for which the absolute difference is minimized



# 5) Are the estimated thresholds different between the two models?



# 6) Predict the default probabilities for the bankloans_missing ( model.predict). Obtain the probabilities of default. 
#    Convert them to booleans (1 or 0) based on either threshold estimated above. 


# 7) Compute summary statistics for the obtained predictions and for the defaults column of bankloans_existing. Compute the fractions of
#    defaults (you may use one of the functions you programmed before). Do the results differ a lot?

# 8) Replace NA's in default columns of bankloans_missing with the predicted values. Append bankloans_missing with bankloans_existing
    

# 9) Save data as bankloans.xlsx with 3 sheets, on the first sheet there should be a dataframe you obtained in part 8 above, sheet 2 
#    must contain bankloans_existing dataframe, and sheet 3 must contain the transposed version of bankloans_missing dataframe





