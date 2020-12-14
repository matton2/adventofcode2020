library(tidyverse)
library(binaryLogic)

day14ex <- read_lines('day14/day14ex.txt')
day14 <- read_lines('day14/day14.txt')


applyMask <- function(number, mask) {
  
  binNum <- fillUpToBit(as.binary(number),36)
  
  for(i in 1:length(mask)) {
    if(!is.na(as.numeric(mask[[i]]))) {
      if(as.numeric(mask[[i]]) != binNum[[i]]) {
        binNum[[i]] <- as.numeric(mask[[i]])
      }
    }
  }
  
  finalNum <- as.numeric(binNum)
  
  
  
}

part1 <- function(thing) {
  
  memPositionTable <- tibble(memPosition = 0, number = 0)
  currentMask <- NA
  
  for(i in 1:length(thing)) {
    
    temp <- str_split(thing[[i]], " = ")[[1]]
    op <- temp[[1]]
    
    if(op == 'mask') {
      currentMask <- str_split(temp[[2]], "")[[1]]
    } else {
      memPositionNew <- str_sub(temp[[1]][[1]], start = 5) %>% str_remove("]") %>% as.numeric()
      finalNumber <- applyMask(as.numeric(temp[[2]]), currentMask)
      if(any(memPositionTable$memPosition %in% memPositionNew)) {
        memPositionTable <- memPositionTable %>% 
          mutate(number = ifelse(memPosition == memPositionNew,finalNumber, number))
      } else {
        tempTibble <- tibble(memPosition = memPositionNew,
                             number = finalNumber)
        
        memPositionTable <- bind_rows(memPositionTable, tempTibble)

      }

    }
    
    
    
  }
  
  return(memPositionTable)
}

example <- part1(day14ex)

finalPart1 <- part1(day14)

print(sum(finalPart1$number), digits = 22)



# part 2

figureOutMemPos <- function(number, mask) {

  
  binNum <- fillUpToBit(as.binary(number),36)
  posToFlip <- c()
  finalNum <- c()
  
  for(i in 1:length(mask)) {
    if(mask[[i]] == "X") {
      posToFlip <- c(posToFlip, i)
    }
    if(!is.na(as.numeric(mask[[i]]))) {
      if(as.numeric(mask[[i]] == 1)) {
        binNum[[i]] <- 1
      }
    }
  }
  
  l <- rep(list(0:1), length(posToFlip))
  
  grid <- expand.grid(l)
  
  temp2 <- data.frame(junk = rep(0,NROW(grid)))
  
  for(j in 1:length(binNum)) {
    if(j %in% posToFlip) {
      pos <- which(posToFlip == j)
      temp2 <- bind_cols(temp2, grid[[pos]])
    } else {
      temp2 <- bind_cols(temp2, rep(binNum[[j]], NROW(grid)))
    }
  }
  
  temp2 <- select(temp2, -junk)
  
  for(i in 1:NROW(temp2)) {
    col <- unname(unlist(temp2[i,]))
    attributes(col) <- list(signed = FALSE, littleEndian = FALSE)
    class(col) <- 'binary'
    finalNum <- c(finalNum, as.numeric(col))
  }
  
  print(finalNum)
  return(finalNum)
  
}


part2 <- function(thing) {
  
  memPositionTable <- tibble(memPosition = 0, number = 0)
  currentMask <- NA
  
  for(i in 1:length(thing)) {
    
    temp <- str_split(thing[[i]], " = ")[[1]]
    op <- temp[[1]]
    
    if(op == 'mask') {
      currentMask <- str_split(temp[[2]], "")[[1]]
    } else {
      
      
      memPositionNew <- str_sub(temp[[1]][[1]], start = 5) %>% str_remove("]") %>% as.numeric()
      memPositions <- na.omit(figureOutMemPos(memPositionNew, currentMask))
      finalNumber <- as.numeric(temp[[2]])
      
      # loop through mem positions and update accordingly
      
      for(i in 1:length(memPositions)) {
        #browser()
        if(any(memPositionTable$memPosition %in% memPositions[[i]])) {
          memPositionTable <- memPositionTable %>% 
            mutate(number = ifelse(memPosition == memPositions[[i]],finalNumber, number))
        } else {
          tempTibble <- tibble(memPosition = memPositions[[i]],
                               number = finalNumber)
          
          memPositionTable <- bind_rows(memPositionTable, tempTibble)
          
        }
      }
      

      
    }
    
    
    
  }
  
  return(memPositionTable)
}

day14ex2 <- read_lines('day14/day14ex2.txt')

example <- part2(day14ex2)
print(sum(example$number), digits = 22)
tictoc::tic()
part2Final <- part2(day14)
print(sum(part2Final$number), digits = 22)
tictoc::toc()