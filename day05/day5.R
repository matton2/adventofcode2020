library(tidyverse)

task5 <- read_csv('day5/day5.csv', col_names = FALSE)

figureOutHalf <- function(numVec, position) {
  
  temp <- length(numVec)
  
  if(temp == 1) {
    return(numVec)
  }
  
  split <- temp/2
  
  if(position == "F" || position == "L") {
    
    return(numVec[1:split])
    
  } else if (position == "B" || position == "R") {
    
    return(numVec[(split+1):temp])
    
  } else {
    stop("Do better")
  }
  
  
  
}


figureOutLocation <- function(rowString, numRows) {
  
  rows <- 0:numRows
  
  for(i in 1:nchar(rowString)) {
    
    rows <- figureOutHalf(rows, str_sub(rowString, i, i))
    
  }
  
  return(rows)
  
}

task5a <- task5 %>% 
  rowwise() %>% 
  mutate(row = figureOutLocation(str_sub(X1, start = 1, end = 7),127),
         seat = figureOutLocation(str_sub(X1, start = 8, end = 10),7),
         seatID = (row*8) + seat)

max(task5a$seatID)

# 2nd part

task5b <- task5a %>% 
  arrange(seatID) %>% 
  ungroup() %>% 
  mutate(different = seatID - lag(seatID)) %>% 
  filter(different > 1)

myseat <- task5b$seatID - 1
