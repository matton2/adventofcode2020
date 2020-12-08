#day 7

library(tidyverse)

rules <- read_lines('day7/day7.txt')
rulesExample <- read_lines('day7/day7Example.txt')

#taking on part 1

totalBags <- c()

findNumberOfBags <- function(rules, color) {
  
  #totalBags <- c()
  
  for(i in 1:length(rules)) {
    
    colorPresent <- str_detect(rules[[i]], color)
    
    if(colorPresent == TRUE) {
      
      temp <- str_split(rules[[i]], " ")
      
      firstColor <- paste(temp[[1]][[1]], temp[[1]][[2]])
      
      if(color == firstColor) {
        
        if(length(totalBags) == 0 || !any(str_detect(totalBags, firstColor))) {
          
          #print(paste('added:', firstColor))
          
          totalBags <<- c(totalBags, firstColor)
          
        } 
        
        
      } else {
        
        findNumberOfBags(rules, firstColor)
        
        
      }
      
    }
    
    
  }
  return(TRUE)
  
  
}

findNumberOfBags(rulesExample, 'shiny gold')

# find the unqiue bags and subtract 1 since in needed to include the original color
totalBags <- c()
findNumberOfBags(rules, 'shiny gold')
part1 <- length(totalBags) - 1 


# taking on part2

part2Example <- read_lines('day7/day7Part2Example.txt')

numberOfBags <- function(rules, color) {
  
  colorRow <- NA
  
  for(i in 1:length(rules)) {
    
    temp <- str_split(rules[[i]], " ")
    
    lineColor <- paste(temp[[1]][[1]], temp[[1]][[2]])
    
    if(color == lineColor) {
      colorRow <- i
      selectedRow <- rules[[i]]
      
      splitRow <- str_split(selectedRow, " ")
      
      if(any(str_detect(selectedRow, "[:digit:]"))) {
        
        for(j in 1:length(splitRow[[1]])) {
          
          if(str_detect(splitRow[[1]][[j]], "[:digit:]")) {
            
            bagsInThisRow <- as.numeric(str_extract(splitRow[[1]][[j]], "[:digit:]"))
            
            nextColor <- paste(splitRow[[1]][[j+1]], splitRow[[1]][[j+2]])
            
            bagsInPrevRow <- numberOfBags(rules, nextColor)
            
            bagsInThisRow <- bagsInThisRow + bagsInThisRow*bagsInPrevRow
            
            print(bagsInThisRow)
          }
          
        }
        
      } else {
        
        return(0)
      }
    }

  }
  
  return(bagsInThisRow)
  #return(bags)
  
}

numberOfBags(part2Example, 'shiny gold')

numberOfBags(rules, 'shiny gold')

#rethinking with a tibble
# 
# tibbleRules <- tibble(x = read_lines('day7/day7.csv'))
# 
# cleanRules <- tibbleRules %>%
#   mutate(x = str_remove_all(x, 'bags'),
#          x = str_remove_all(x, 'bag'),
#          x = str_remove_all(x, "\\.")) %>%
#   separate(col = x, into = c('outer', 'inside'), sep = 'contain') %>%
#   separate(col = inside, into = c('bag1', 'bag2', 'bag3', 'bag4'), sep = ",") %>%
#   mutate(across(.fns = str_trim)) %>% 
#   separate(col = bag1, into = c('num1', 'col1'), sep = " ", extra = 'merge') %>% 
#   separate(col = bag2, into = c('num2', 'col2'), sep = " ", extra = 'merge') %>% 
#   separate(col = bag3, into = c('num3', 'col3'), sep = " ", extra = 'merge') %>% 
#   separate(col = bag4, into = c('num4', 'col4'), sep = " ", extra = 'merge')
# 
# graph <- cleanRules %>% 
#   select(outer, num1, col1) %>% 
#   bind_rows(., (select(cleanRules, outer, num1 = num2, col1 = col2))) %>% 
#   bind_rows(., (select(cleanRules, outer, num1 = num3, col1 = col3))) %>% 
#   bind_rows(., (select(cleanRules, outer, num1 = num4, col1 = col4))) %>% 
#   select(from = outer, weight = num1, to = col1) %>% 
#   mutate(weights = as.numeric(weight))
# 
# vertices <- unique(graph$from)
# 
# library(igraph)
# 
# relations <- graph_from_data_frame(graph, vertices = vertices)
# 
# dfs(relations, root = 'shiny gold')
# 
# all_shortest_paths(relations, 'shiny gold')
# 
# color <- 'shiny gold'
# 
# #first part
# 
# 
# part1 <- cleanRules %>% 
#   filter(col1 == color | col2 == color | col3 == color | col4 == color)
# 
# noIdea <- filter(cleanRules, outer == 'shiny gold')
# 
# filter(cleanRules, outer == noIdea$col1)
# 
# 
# 
