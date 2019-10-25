library(magrittr)
library(tidyverse)

# DATA --------------------------------------------------------
# https://toolbox.google.com/datasetsearch/search?query=sales&docid=mEb0PRlWa0KIiHwzAAAAAA%3D%3D

# READ BY YEAR ------------------------------------------------------------

files = list.files("data/yearly", full.names = T)

# the for loop way
for (file in files) {
    if (exists("games")) {
        games = rbind(games, read_csv(file))
    } else {
        games = read_csv(file)
    }
}

# vectorised
games = map_df(files,
               ~read_csv(.x, col_types = cols(.default = "c")))


# PARSE NUMERIC FEATURES --------------------------------------------------

# R is functional programming language
check_numeric = function(col) {
    nas =
        col %>%
        {.[!is.na(.)]} %>% 
        as.numeric() %>% 
        is.na() %>%
        sum()
    
    if (nas == 0) {
        col = as.numeric(col)
    }
    
    return(col)
}

# loopy way
for (i in colnames(games)) {
    games[,i] = check_numeric(pull(games[,i]))
}

head(games(20))

# vectorised
games %<>% 
    map_df(check_numeric)


# READ BY YEAR FUNCTION ---------------------------------------------------

read_by_year = function(path, years) {
    files = list.files(path)
    
    if (is.numeric(years) | is.character(years)) {
        pattern = paste0("game_sales", years, ".csv")
        files = files[files %in% pattern]
        
        writeLines(paste0("Reading files:\n",
                     paste(files, collapse = "\n")
                     )
              )

            } else if (year != "all") {
            } else {
        stop("Please specify years as numeric or character vector, or use 'all' to use all files")
    } # WHAT IS WRONG WITH THIS?
    
    input =
        files %>% 
        {file.path(path, .)} %>% 
        map_df(~read_csv(.x, col_types = cols(.default = "c"))) %>% 
        map_df(check_numeric)
    
    return(input)
}

games_2k = read_by_year("data/yearly", 2000:2030)


# Selection ----------------------------------------------------

games_by_year = 
    games %>% 
    select(Global_Sales, Year_of_Release, Critic_Score, User_Score) %>% 
    na.omit() %>% 
    split(.$Year_of_Release)


games %>% 
    drop_na() %>% 
    filter(Year_of_Release > 1994) %>% 
    group_by(Year_of_Release) %>% 
    summarise(critics = min(Critic_Score, na.rm = T),
              users = min(User_Score, na.rm = T)) %>% View()

