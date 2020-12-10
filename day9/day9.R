library(tidyverse)

day9 <- read_lines('day9/day9.txt') %>% 
  as.numeric()

day9Ex <- read_lines('day9/day9Ex.txt') %>% 
  as.numeric()


part1 <- function(data, pream) {
  
  brokenOne <- NA
  
  for(i in (pream+1):length(data)) {
    
    curNum <- data[[i]]
    
    #print(paste('currentnum',day9Ex[[i]]))
    
    toSearch <- data[(i-pream):(i-1)]
    foundOne <- FALSE
    while(foundOne == FALSE) {
      for(j in 1:length(toSearch)) {
        target <- curNum - toSearch[[j]]
        for(k in j:length(toSearch)) {
          if(toSearch[[k]] == target) {
            #print('found one')
            foundOne <- TRUE
          }
          
        }
      }
      
      if(foundOne == FALSE) {
        print(paste('didnt find one:', curNum))
        brokenOne <- curNum
        foundOne <- TRUE
      }
      
    }
    
    
  }
  
  return(brokenOne)
  
}


part1(day9Ex, 5)
part1 <- part1(day9, 25)

part2 <- function(data, pream) {
  
  brokenOne <- NA
  found <- c()
  
  for(i in (pream+1):length(data)) {
    
    curNum <- data[[i]]
    
    #print(paste('currentnum',day9Ex[[i]]))
    
    toSearch <- data[(i-pream):(i-1)]
    foundOne <- FALSE
    while(foundOne == FALSE) {
      for(j in 1:length(toSearch)) {
        target <- curNum - toSearch[[j]]
        for(k in j:length(toSearch)) {
          if(toSearch[[k]] == target) {
            #print('found one')
            foundOne <- TRUE
          }
          
        }
      }
      
      if(foundOne == FALSE) {
        print(paste('didnt find one:', curNum))
        brokenOne <- curNum
        foundOne <- TRUE
      }
      
    }
    
    
  }
  
  for(m in 1:length(data)) {
    for(n in m:length(data)) {
      found <- data[m:n]
      sum <- sum(found)
      #print(sum)
      if(brokenOne == sum) {
        return(found)
      }
    }
  }
  
  
}


part2(day9Ex, 5)
part2Ans <- part2(day9, 25)

finalPart2 <- min(part2Ans) + max(part2Ans)
finalPart2
