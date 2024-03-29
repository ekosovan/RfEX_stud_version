install.packages(c('jsonlite','readxl', 'writexl'))

library(magrittr)
library(tidyverse)


# ORIENTATION -------------------------------------------------------------
getwd()
list.files("data")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ IMPORT #############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CSV ------------------------------------------------------------------
# csv = comma separated values
# a simple flatfile:
#       columns separated by comma ','
#       decimals separated by dot '.'
#       rows separated by newline '\n'
# fied description (colnames) non-mandatory
csv = read_csv("data/london_2014-05.csv")
head(csv)

# A special sub-type is CSV saved from Slav version of Excel (yes, Excel hates you)
# looks like csv, but has different separators:
#       columns separated by semicolon ';'
#       decimals separated by comma ','
csv2 = read_csv2("data/london_2014-05.csv2")
head(csv2)


# XLS ---------------------------------------------------------------------
# Excel based binary files
# widely used everywhere
# tabular content
library(readxl)
xls = read_excel("data/econmap.xlsx", sheet = 1)
head(xls)


# JSON ---------------------------------------------------------------------
library(jsonlite)
json = read_file("data/fiscal2017.json")
json = fromJSON(json)

json$data %>% View()

# ANY TABULAR FLATFILE ----------------------------------------------------
# any flatfile with rigid separators
del = read_delim("data/london_2014-05.tsv", delim = "\t")


# RDATA / RDS -------------------------------------------------------------------
# saving R objects (whatever in your Environment)
# good for saving models and other special (non-tabular) data
load("data/london.RData") # no assignment, dangerous!
rds = readRDS("data/london.RDS")


# DATASETS ----------------------------------------------------------------
# london_2014-05       https://toolbox.google.com/datasetsearch/search?query=econometrics%20csv&docid=2U0qJW26x%2B%2BMJDVlAAAAAA%3D%3D
# econmap              https://toolbox.google.com/datasetsearch/search?query=econometrics%20xlsx&docid=LY85I4W1qT8Xe8L9AAAAAA%3D%3D
# fiscal2017           https://toolbox.google.com/datasetsearch/search?query=economics%20json&docid=QU9%2F0grLqAZqGRCGAAAAAA%3D%3D


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ EXPORT #############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CSV ---------------------------------------------------------------------
write_csv(csv, "data/london_2014-05.csv")


# JSON --------------------------------------------------------------------
write_json(json, "data/fiscal2017.json")


# RDATA / RDS -------------------------------------------------------------
save(rdata, file = "data/london.RData")
saveRDS(rds, file = "data/london.RDS")

# XLS
library(writexl)
write_xlsx(xls, "data/econmap.xlsx")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ MUTATE & AGGREGATE #############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list.files("data")
rm(list = ls())

sales = read_csv("data/sales_data_sample.csv")
# https://toolbox.google.com/datasetsearch/search?query=sales&docid=1%2BevukXLxiyX4xlWAAAAAA%3D%3D
head(sales)
summary(sales)


# REGIONS -----------------------------------------------------------------
sales %>% 
    pull(TERRITORY) %>% 
    unique()

# what are the countries with no region value (NA)?
na_territory = 
    sales %>% 
    filter(is.na(TERRITORY)) %>% 
    pull(COUNTRY) %>% 
    unique()
na_territory

# are all of those countries without region?
sales %>% 
    filter(COUNTRY %in% na_territory) %>% 
    pull(TERRITORY) %>% 
    unique()

# let's fill in the missing values
sales %<>% 
    mutate(TERRITORY = ifelse(COUNTRY %in% na_territory, "AMER", TERRITORY))

# SALES DECOMPOSITION PER TERRITORY ---------------------------------------
# what is the customer behaviour per territory?


sales %>% 
    group_by(TERRITORY, ORDERNUMBER) %>% 
    summarise(basket = sum(QUANTITYORDERED),
              price = mean(PRICEEACH)) %>% 
    ungroup() %>% 
    group_by(TERRITORY) %>% 
    summarise(basket = mean(basket),
              price = mean(price),
              visits = length(ORDERNUMBER))

# SALES PER YEAR ----------------------------------------------------------
# the first overvie, what are the sales each year?
sales %>% 
    group_by(YEAR_ID) %>% 
    summarise(sales = sum(SALES))

# how come the year 2005 has such a low sales?
sales %>% 
    filter(YEAR_ID == "2005") %>% 
    pull(MONTH_ID) %>% 
    unique()

# what is the usual sales share in the first 5 months?
sales %>% 
    group_by(YEAR_ID, MONTH_ID) %>% 
    summarise(sales = sum(SALES)) %>% 
    ungroup() %>% 
    mutate(first5 = ifelse(MONTH_ID <= 5, 1, 0)) %>% 
    group_by(YEAR_ID, first5) %>% 
    summarise(sales = sum(sales)) %>% 
    ungroup() %>% 
    group_by(YEAR_ID) %>% 
    summarise(sales_shr = sum(first5 * sales) / sum(sales))

# what is the projection for 2005?
sales %>% 
    group_by(YEAR_ID) %>% 
    summarise(sales = sum(SALES)) %>%
    mutate(sales = ifelse(YEAR_ID == "2005", sales / 0.25, sales))


    