library('magrittr')
romeo = readLines("http://www.gutenberg.org/cache/epub/1112/pg1112.txt", encoding = "UTF8")
romeo %<>% {.[. != ""]}
first_line = which(romeo[1:100] == "1595")
romeo = romeo[-1:-first_line]
persons = romeo[4:28]
corpus = romeo[-1:-32]



library(stringr)
corpus[1:4] %>% 
  print() %>% 
  str_length() # or "nchar()" from base R


(prol = str_sub(corpus[1], 29, 36)) # or "substring()" from base R

paste("the", prol, sep = "_||_") %>% #default sep is whitespace
  print() %>% 
  tolower() %>% # or "stringr::str_to_lower()"
  print() %>% 
  toupper() # or "stringr::str_to_upper()"

paste0("the //__//", prol) # no separator

file.path("w7", "data", "text.csv") # file path wrapper utility


head(persons, 3) %>% 
  print() %>% 
  str_split(",",n = 2) # split only once

corpus[1] %>%
  print() %>% 
  str_trim() # take away the preceding and proceeding whitespaces

str_trim("  abc      def  ")
str_squish("  abc      def  ")


(jap = readLines("japanese.txt"))

# iconvlist()
iconv(jap, "x-mac-japanese", "UTF-8")


files = paste0("file_", c(0,1,10,100,1000), "_", 2010:2014, ".csv"))
str_sub(files, 8, 11)


str_detect("abcd", "a") # grep / grepl

str_locate("abcd", "a")

str_extract("abcd", "a") # gsub

str_replace("abcd", "ab","d") # gsub

str_remove("abcd", "a") # gsub

#"." any character
str_replace("abcd_e",".","g")

# "*" any number of times
str_extract("abc_de","_.*")

# "?" at most once
str_extract("abc_de","_.?")

# "+" at least once
str_extract("abc_de","_.+")

# "^" start of string
str_replace_all("abcd_a","^a","g")

# "$" end of string
str_replace_all("abcd_ab","a$","g")

# "|" RegEx or 
str_extract(c("abc","cd"),"ab|c")

# "{}" number of times
str_extract("aaaabcde","a{1,3}")

str_extract("aaaacde","a{2,}")

# "[]" set of possibilities
str_extract(c("aabcde"),"[ab]{3}")

# "()" glue characters
str_extract(c("ababcdab"),"(ab){2}")

str_extract("abc123_ ?!<>","[:alnum:]*") # or [:alpha:] or [:digit:] only

str_replace_all("abc123_ ?!","[:punct:]","X") # special characters

str_extract("abc123_ ?!<>","[:space:]") # whitespace, newlines, ...

str_replace("abc123_ ?!<>","\\s","XX") # special characters (whitespace in this case)

str_extract("$?+.",".")

str_extract("$?+.","\\.")

(files = paste0("file_", c(0,1,10,100,1000), "_", 2010:2014, ".csv"))