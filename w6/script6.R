library(magrittr)
library(tidyverse)


# PREPARE THE DATA --------------------------------------------------------
# https://toolbox.google.com/datasetsearch/search?query=Japan%20Trade%20Statistics&docid=wssrt0mVG%2F6fuGBBAAAAAA%3D%3D

list.files("data")

zipped = unzip("data/ym_2018.csv.zip", list = TRUE)
zipped

japan <- read_csv(unz("data/ym_2018.csv.zip", zipped$Name),
                  col_types = paste0(rep("c", 13), collapse = ""))

summary(japan)
head(japan)

# standardise colnames
colnames(japan) %>% tolower() %>% unique() %>% length() == ncol(japan)
colnames(japan) %<>% tolower()
head(japan)
# numeric values
japan$value %<>% as.numeric()
japan$value %>% summary()

# check import definition
japan %>%
    pull(exp_imp) %>% 
    unique()

# because builtin functions
na_dif = function(x, y){
    x[is.na(x)] = 0
    y[is.na(y)] = 0
    return(x - y)
}

# OVERVIEWS ---------------------------------------------------------------
# what is the export/import and the trade deficite per commodity type and country?
EI = 
    japan %>% 
    select(exp_imp, country, hs2, value) %>% 
    group_by(country, hs2, exp_imp) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(exp_imp = ifelse(exp_imp == 1, "export", "import")) %>% 
    spread(exp_imp, value) %>% 
    mutate(dif_na = export - import,
           dif = na_dif(export, import))
EI

# report the export, import and trade performance for each commodity across the countries
# performance is defined as percentage of the mean commodity sales (in all countries)

EI_compare = 
    EI %>% 
    select(-dif_na) %>% 
    gather(EI, value, export:dif) %>% 
    group_by(hs2) %>% 
    mutate(comparison = value / mean(value, na.rm = T)) %>% 
    select(- value) %>% 
    spread(EI, comparison)

list.files("data")

# read the information about the countries and commodities
countries = read_csv("data/country_eng.csv", col_types = "ccc")
head(countries)
colnames(countries) %<>% tolower()


h2 = read_csv("data/hs2_eng.csv")
head(h2)

# large outliers
EI_out = 
    EI_compare %>% 
    filter(abs(dif) > 30)

# add country identifiers
EI_out %>% 
    full_join(countries) %>% View()

EI_out %>% 
    left_join(countries) %>% View()

# & add commodity description

EI_out %>% 
    left_join(countries, by = c("country" = "country")) %>% 
    inner_join(h2) %>% 
    View()
