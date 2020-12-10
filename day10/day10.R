library(tidyverse)

day10 <- tibble(x = c(as.numeric(read_lines('day10/day10.txt')),0))
# day10Ex <- tibble(x =c(as.numeric(read_lines('day10/day10Ex1.txt')),0))
# 
# part1ex <- c(16,
#              10,
#              15,
#              5,
#              1,
#              11,
#              7,
#              19,
#              6,
#              12,
#              4,22)
# 
# ex1Sorted <- sort(part1ex)
# 
# ex1Sorted



#part 1

day10Pt1 <- day10 %>% 
  arrange(x) %>% 
  mutate(diff = x - lag(x))


table(day10Pt1$diff)

# part 2

#some weird math to figure out if that path works?
num <- c(1:175)

#some weird math to figure out if that path works?
num <- c(1:max(day10Pt1$x))

part2 <- tibble(num = num) %>% 
  full_join(day10Pt1, by = c('num' = 'x')) %>% 
  mutate(diff = if_else(is.na(diff), 0, diff))


diff <- part2$diff

totalCount <- c(1,2,3,7) # need to seed the function with the beginning of the cumsum vector

part2Func <- function(diff, startingCount) {
  
  totalCount <- startingCount
  
  for(i in 5:length(diff)) {
    #print(diff[[i]])
    if(diff[[i]] == 0) {
      totalCount <- c(totalCount, 0)
      #print(totalCount)
    } else {
      
      toAdd <- totalCount[[(i-1)]] + totalCount[[(i-2)]] + totalCount[[(i-3)]]
      #print(toAdd)
      
      totalCount <- c(totalCount, toAdd)
      #print(totalCount)
    }
    
  }
  
  return(totalCount[[(length(diff)-1)]])
  
}




print(part2Func(diff, totalCount), digits = 22)
