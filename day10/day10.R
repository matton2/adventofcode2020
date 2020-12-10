library(tidyverse)

day10 <- tibble(x = c(as.numeric(read_lines('day10/day10.txt')),0))
day10Ex <- tibble(x =c(as.numeric(read_lines('day10/day10Ex1.txt')),0))

#part 1

day10Pt1 <- day10 %>% 
  arrange(x) %>% 
  mutate(diff = x - lag(x))


table(day10Pt1$diff)

# part 2

day10Pt2 <- read_lines('day10/day10.txt') %>% 

totalCount <- 0

part2 <- function(dataVec) {
  
  for(i in 1:length(dataVec)) {
    
    
    
  }
  
  
}

