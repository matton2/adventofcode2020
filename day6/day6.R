library(tidyverse)

task6 <- read_csv("day6/day6_spaces.csv", col_names = FALSE) %>% 
  select(X1)

groupCount <- 1
groups <- c()

for (i in 1:NROW(task6)) {
  
  if (!is.na(task6$X1[[i]])) {
    groups <- c(groups, groupCount)
  } else {
    groups <- c(groups, NA)
    groupCount <- groupCount + 1
  }
  
}

task6a <- task6 %>% 
  mutate(groupNumber = groups) %>% 
  filter(!is.na(X1)) %>% 
  group_by(groupNumber) %>% 
  summarize(text = paste(X1, collapse = ""),
            answers = length(unique(str_split(text[[1]], "")[[1]])))

sum(task6a$answers)

# onto task 2

task6b <- task6 %>% 
  mutate(groupNumber = groups) %>% 
  filter(!is.na(X1)) %>% 
  group_by(groupNumber) %>% 
  pivot_wider(names_from = groupNumber, values_from = X1)

countOfLetters <- c()

for(i in 1:NCOL(task6b)) {
  
  letterCount <- 0
  
  for(j in 1:length(letters)) {
    
    searchLetter <- letters[[j]]
    
    temp <- c()
    
    for(k in 1:length(task6b[,i][[1]][[1]])) {
      
      there <- str_detect(task6b[,i][[1]][[1]][[k]], searchLetter)
      
      temp <- c(temp, there)
      
    }
    
    if(all(temp)) {
      letterCount <- letterCount + 1
    }
    
  }
  
  countOfLetters <- c(countOfLetters, letterCount)
}