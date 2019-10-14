library(magrittr)

# 1.1
print(Sys.time()) # the print function is optional

# 1.2
bad_list = list(na = rep(NA, 3),
                chars = c("a", "b", "c"),
                bools = c(F, F, T))

# 1.3
happy_frame = data.frame(na = rep(NA, 3),
                         chars = c("a", "b", "c"),
                         bools = c(F, F, T))

# 2 setup
fruit_size = c(2, 4, 7, 5, 3, 2)
fruit_col = c('g', 'y', 'r', 'g', 'b', 'r')

# 2.1 (based on fruit_size)
sum(fruit_size) / mean(fruit_size)

# 2.2 (based on fruit col)
seq_along(fruit_size) %% 3 == 0

# 2.3 (brackets optional, but suggested)
(fruit_size > 5 & fruit_col == "red") | (fruit_size < 5 & fruit_col != "red")

# 3.1
mtcars = mtcars

mtcars[mtcars$carb <= 2,][c(3,8,12), "disp"] %>% sum() # conscise way, using only baseR and magrittr


library(tidyverse) # tidyverse way
mtcars %>% 
    filter(carb <= 2) %>% 
    pull(disp) %>% 
    .[c(3,8,12)] %>% # "." stands for "place previous value here"
    sum()

# 3.2
test = list(a = mtcars)

# 3.2.1
test[[1]][5,3]

# 3.2.2
test$a$drat[8]
