library(tidyverse)

day19Rules <- read_lines('day19/day19.txt', n_max = 138)

day19input <- read_lines('day19/day19.txt', skip = 139)



orderedRules <- function(rules) {
  orderedRules <- rep(NA, length(rules))
  
  for(i in 1:length(rules)) {
    
    rule <- str_split(rules[[i]], ":")[[1]]
    
    orderedRules[[(as.numeric(rule[[1]]) + 1)]] <- str_trim(rule[[2]], side = "both")
    
  }
  
  return(orderedRules)
  
}

inputRules <- orderedRules(day19Rules)

generateRuleStrings <- function(orderedRules) {
  
  
  
  
  
}


