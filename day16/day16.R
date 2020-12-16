library(tidyverse)

rules <- read_lines('day16/day16.txt', n_max = 20)
rules

tickets <- read_csv('day16/day16.txt', skip = 25, col_names = FALSE)

#part 1
figureOutRules <- function(rules) {
  completeRules <- c()
  
  for(i in 1:length(rules)) {
    temp <- str_remove_all(rules[[i]], "[:alpha:]") %>% 
      str_remove_all(., ":") %>% 
      str_split(., " ")
    
    temp <- temp[[1]]
    
    for(j in 1:length(temp)) {
      if(str_detect(temp[[j]], "[:digit:]")) {
        num1 <- str_split(temp[[j]], "-")[[1]][[1]] %>% as.numeric()
        num2 <- str_split(temp[[j]], "-")[[1]][[2]] %>% as.numeric()
        newList <- eval(parse(text = "c(num1:num2)"))
        completeRules <- c(completeRules, newList)
      }
    }
    
    
  }
  
  completeRules <- unique(completeRules) %>% sort()
  
}

allRules <- figureOutRules(rules)

figureOutBadTickets <- function(tickets, rules) {
  
  invalidTickets <- c()
  
  for(i in 1:NCOL(tickets)) {
    badPositions <- which(!(tickets %>% pull(i) %in% rules))
    for(j in 1:length(badPositions)) {
      invalidTickets <- c(invalidTickets, (tickets %>% pull(i))[[badPositions[[j]]]])
    }
  }
  
  return(sum(invalidTickets))
  
}

figureOutBadTickets(tickets, allRules)

# part 2

# we need to remove bad tickets first

clean <- function(tickets, rules) {
  
  cleanTickets <- c()
  
  for(i in 1:NROW(tickets)) {
    badPositions <- which(!(tickets %>% slice(i) %in% rules))
    if(length(badPositions) == 0) {
      cleanTickets <- c(cleanTickets, i)
    }
    
  }
  
  return(tickets %>% slice(cleanTickets))
  
  
}

myTicket <- read_csv('day16/day16.txt', col_names = FALSE, skip = 22, n_max = 1)

cleanTickets <- clean(tickets, allRules) %>% bind_rows(myTicket)


# need to figure out how to clean up the rules
figureOutRules <- function(rules) {
  completeRules <- list()
  
  for(i in 1:length(rules)) {
    temp <- str_remove_all(rules[[i]], "[:alpha:]") %>% 
      str_remove_all(., ":") %>% 
      str_split(., " ")
    
    temp <- temp[[1]]
    
    for(j in 1:length(temp)) {
      if(str_detect(temp[[j]], "[:digit:]")) {
        num1 <- str_split(temp[[j]], "-")[[1]][[1]] %>% as.numeric()
        num2 <- str_split(temp[[j]], "-")[[1]][[2]] %>% as.numeric()
        newList <- eval(parse(text = "c(num1:num2)"))
        completeRules <- c(completeRules, newList)
      }
    }
    
    
  }
  
  completeRules <- unique(completeRules) %>% sort()
  
}



