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

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(reshape2)
library(ggplot2)

data <- read_csv("bankloans.csv")
str(data)
head(data)

# 1) Create a function 'var_summary' that has column (or vector) as an input and returns you a table with 
#     number of observations, number of missing values, sum of all values, mean, standard deviation,
#     variance, minimum, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95 and 0.99 quantiles and maximum
#     i.e. extended descriptive statistics

var <- c(1,2,7,5,8,4,0,9,2,3,4,1,3,7,3,4)

var_summary <- function(d){
  description <- describe(d)
  summary <- summary(d)
  quantiles <- quantile(d, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95,0.99))
  return(description)
  return(summary)
  return(quantiles)
}

var_summary(var)


# 2) Create a function 'create_dummies' that has two inputs - a dataframe and column_name
#   The function has to create dummy variables for categorical data stored in column_name, i.e.
#   suppose you have a single column  c('a','b','c','a') the return of the function has to produce 4x3 matrix with
#   a b c
#   1 0 0
#   0 1 0
#   0 0 1
#   1 0 0
#   The output of the function should be an input dataframe with dummies matrix added as new columns

input <- data.frame(c('a','b','c','a'))
names(input) <- c("value")

input%>%mutate(rn = 1)%>%mutate(id = row_number())%>%spread(value,rn,fill=0)

create_dummies <- function(d,v){
  out <- input%>%
    mutate(rn = 1)%>%mutate(id = row_number())%>%spread(v,rn,fill=0)%>%
    select(-rn)
  return(out)
}

create_dummies(input,value)


# 3) Create a function 'outlier_capping' that would have vector or column as inputs. The function then 
#    replaces all right tail outliers (very huge numbers) with 0.99 quantile and all 
#    left tail outliers (big negative numbers) with 0.01 quantile of the column.
#    The function should output the same column\vector with processed outliers    

v <- c(0.1,5,6,7,8,20)


outlier_capping <- function(v){
  k <- quantile(v, probs = c(0.01, 0.99))
  x <- which.max(v>k)
  y <- which.min(x>k)
  v[x] <- k[2]
  v[y] <- k[1]
  return(v)
}

outlier_capping(v)

# 4) Create a function 'process_missing' that would have a column\vector as an input. The function then
#    replaces all missing values with the mean and returns the processed column\vector of data.

v <- c(0,NA,1,2,5,7,NA)

process_missing <- function(v){
  mean <- mean(v,na.rm=TRUE)
  v <- v%>%replace_na(mean)
  return(v)
}

process_missing(v)

# 5) Apply the 'var_summary' function to each column of the data. (Use purrr maps or lapply or sapply - whichever you prefer). Any missing data?

var_summary <- function(v){
  k <- lapply(v, summary,is.na())
  return(k)
}

var_summary(data)

# 6) If everything is done correctly, there should be 150 NA's in 'default' column. Separate the dataframe into two
#    bankloans_existing and bankloans_new based on whether default value is present or missing

bankloans_existing <- data%>%filter(is.na(default)==FALSE)
bankloans_new <- data%>%filter(is.na(default)==TRUE)

# 7) Apply outlier_capping and process_missing on every column of bankloans_existing

bankloans_existing <- lapply(bankloans_existing,outlier_capping)

bankloans_existing <- lapply(bankloans_existing,process_missing)

bankloans_existing <- as.data.frame(bankloans_existing)

# 8) Calculate the correlation matrix with correlations represented by color (google heatmap, there is really simple solution using on
#    of the packages mentioned in the course)

cormat <- round(cor(bankloans_existing),2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# 9) Create a 2x4 matrix of boxplots where x = default, y is all other columns (all 8 plots should be on one picture)


bankloans_existing_new <- bankloans_existing  %>%
  gather(key = serie, value =value,-default )



ggplot(bankloans_existing_new,aes(x=factor(default),y=value))+facet_wrap(~serie,ncol = 2,scales='free')+geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2, notch=FALSE)


# 10) Create histogram of employ for those who defaulted (default = 1) and not (default  = 0) on the same graph (use color to distinguish 
#     defaulted and nondefaulted). Make histogram a bit transparent by setting alpha to 0.5 (hope you remember how to do it)

ggplot(bankloans_existing, aes(x = employ,color=factor(default))) +
  geom_histogram()

##############################################################################
# Part 2: ITS LOGIT TIME
##############################################################################

# 1) Estimate a logistic regression (google glm) of default on all other variables (use bankloans_existing data).
#    To do that you need to run the following commands

model = glm(bankloans_existing$default ~ bankloans_existing$age + bankloans_existing$ed + bankloans_existing$employ + bankloans_existing$address + bankloans_existing$income + bankloans_existing$debtinc + bankloans_existing$creddebt + bankloans_existing$othdebt)
model

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

i=0.05 #probability
step=0.05
end=1
accuracy_f <- c()
probability <- c()
accuracy <- function(d,p){
  for (k in 1:round((end-i)/step)){
  table <- table(d, model$fitted.values  > i)
  tp=table[1,1]
  fp=table[1,2]
  fn=table[2,1]
  tn=table[2,2]
  accuracy_f[k] = (tp+tn)/(tp+fn+fp+tn)
  probability[k]=i
  i=i+step
  }
  return(accuracy_check=data.frame(accuracy_f,probability))
}

result <- accuracy(bankloans_existing$default,i)
result

result%>%filter(accuracy_f==max(accuracy_f))

# 4) Another (and even more popular choice) is Specifity - Sesitivity score. Optimal threshold is chosen as one for which
#    the two the absolute difference abs(Specifity - Sensitivity) is closest to zero
#    Specifity  = TN/(TN+FP)
#    Sensitivity  = TP/(TP+FN)
#    Create a function that would calculate Specifity and Sensitivity for a the same grid as in part 3. The function should output
#    the threshold value for which the absolute difference is minimized

i=0.05 #probability
step=0.05
end=1
diff <- c()
score <- function(d,p){
  for (k in 1:round((end-i)/step)){
    table <- table(d, model$fitted.values  > i)
    tp=table[1,1]
    fp=table[1,2]
    fn=table[2,1]
    tn=table[2,2]
    specifity=tn/(tn+fp)
    sensitivity=tp/(tp+fn)
    diff[k] = abs(specifity - sensitivity)
    probability[k]=i
    i=i+step
  }
  return(accuracy_check=data.frame(diff,probability))
}

result <- score(bankloans_existing$default,i)
result

result%>%filter(diff==min(diff))

# 5) Are the estimated thresholds different between the two models?

#Yes, for accuracy the probability was 0.45, for scoring 0.55

# 6) Predict the default probabilities for the bankloans_missing ( model.predict). Obtain the probabilities of default. 
#    Convert them to booleans (1 or 0) based on either threshold estimated above. 

#model = glm(bankloans_existing$default ~ bankloans_existing$age + bankloans_existing$ed + bankloans_existing$employ + bankloans_existing$address + bankloans_existing$income + bankloans_existing$debtinc + bankloans_existing$creddebt + bankloans_existing$othdebt)
#model

#data <- model[["coefficients"]]

#predict=data[1]+data[2]*bankloans_new$age + data[3]*bankloans_new$ed + data[4]*bankloans_new$employ + data[5]*bankloans_new$address + data[6]*bankloans_new$income + data[7]*bankloans_new$debtinc + data[8]*bankloans_new$creddebt + data[9]*bankloans_new$othdebt

#bankloans_new$default=predict
#bankloans_new$default = ifelse(predict >= 0.5, 1, 0)

predict <-  predict(model, bankloans_new[,1:8], type = "response")
predict <- ifelse(predict>0.45,1,0)
predict <- as.data.frame(predict)
names(predict) <- c("default")


# 7) Compute summary statistics for the obtained predictions and for the defaults column of bankloans_existing. Compute the fractions of
#    defaults (you may use one of the functions you programmed before). Do the results differ a lot?
install.packages("Hmisc")
library(Hmisc)
summary(bankloans_new$default)
describe(bankloans_new$default)
describe(bankloans_existing$default)

bankloans_existing%>%count(default)%>%mutate(percentage=n/sum(n))
predict%>%count(default)%>%mutate(percentage=n/sum(n))

# 8) Replace NA's in default columns of bankloans_missing with the predicted values. Append bankloans_missing with bankloans_existing

bankloans_new$default <- predict



# 9) Save data as bankloans.xlsx with 3 sheets, on the first sheet there should be a dataframe you obtained in part 8 above, sheet 2 
#    must contain bankloans_existing dataframe, and sheet 3 must contain the transposed version of bankloans_missing dataframe
install.packages("rjava")
install.packages("xlsxjars")
install.packages("xlsx")
library(xlsxjars)
library(xlsx)

final_data = data.frame(rbind(bankloans_existing,bankloans_new))

# Write the first data set in a new workbook
write.xlsx(bankloans_new, file = "bankloans.xlsx",
           sheetName = "bankloans_new", append = FALSE)
# Add a second data set in a new worksheet
write.xlsx(bankloans_existing, file = "bankloans.xlsx", 
           sheetName="bankloans_existing", append=TRUE)
# Add a third data set
write.xlsx(final_data, file = "bankloans.xlsx",
           sheetName="final_data", append=TRUE)
