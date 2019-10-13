# Problem 1
# print actual time

Sys.time()

# create a list ”bad list” with 3 positions

badlist <- list(na = c(NA, NA, NA), chars = c("a", "b", "c"), bools = c(FALSE, FALSE, TRUE))
badlist

# create a data frame

happyframe <- data.frame(na = c(NA, NA, NA), chars = c("a", "b", "c"), bools = c(FALSE, FALSE, TRUE))
happyframe

# Problem 2
# lenght of numeric vector input
x <- c(1, 2, 3, 4)
length(x)

# not using len function
vector <- c(1,2,3,4,5,6,7,8,9)
diff(range(vector))+1

# boolean vector
ifelse(vector %% 3 == 0, "TRUE", "FALSE")

# fruits
ifelse(vector > 5, "RED", "NON-RED")


# Problem 3
# load mtcars dataset

df = mtcars

# slice rows where columns carb is less than or equal to 2
filter(mtcars, carb <= 2)

# choose rows 3, 8, 12
mtcars %>% slice(3, 8, 12)

# sum the values of the column disp
sum(mtcars$disp)

# create a list
list(mtcars[[3]][5], mtcars$drat[8])