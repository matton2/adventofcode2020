#day 7

library(tidyverse)

rules <- read_lines('day7/day7.csv')
rulesExample <- read_lines('day7/day7Example.txt')

totalBags <- c()

findNumberOfBags <- function(rules, color) {
  
  #totalBags <- c()

  for(i in 1:length(rules)) {
    
    colorPresent <- str_detect(rules[[i]], color)
    
    if(colorPresent == TRUE) {
      
      temp <- str_split(rules[[i]], " ")
      
      firstColor <- paste(temp[[1]][[1]], temp[[1]][[2]])
      
      if(color == firstColor) {
        #bags <- str_count(rules[[i]], "[:digit:]")
        
        if(length(totalBags) == 0 || !any(str_detect(totalBags, firstColor)))
          
          print(paste('added:', firstColor))
          
          totalBags <<- c(totalBags, firstColor)
        
      } else {
        
        findNumberOfBags(rules, firstColor)
        
        
      }
      
      
    }
    
  }
  
  return(totalBags)
  
}

findNumberOfBags(rulesExample, 'shiny gold')

sum(findNumberOfBags(rules, 'shiny gold'))
