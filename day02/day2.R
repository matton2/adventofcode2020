library(tidyverse)

task2 <- read_csv("day2/day2.csv", col_names = FALSE) %>% 
  select(X1) %>% 
  separate(col = X1, into = c('num', 'key', 'password'), sep = " ") %>% 
  separate(col = num, into = c('min', 'max'))


# first part

task2a <- task2  %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         key = str_remove(key, ":"),
         keysInPass = str_count(password, pattern = key),
         valid = if_else(min <= keysInPass & keysInPass <= max, TRUE, FALSE))

sum(task2a$valid)

# second part

task2b <- task2 %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         key = str_remove(key, ":"),
         pos1 = str_sub(password, start = min, end = min),
         pos2 = str_sub(password, start = max, end = max),
         contains1 = if_else(pos1 == key | pos2 == key, TRUE, FALSE),
         contains2 = if_else(pos1 == key & pos2 == key, TRUE, FALSE),
         valid = if_else(contains1 == TRUE & contains2 == FALSE, TRUE, FALSE))

sum(task2b$valid)
