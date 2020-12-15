library(tidyverse)


part1 <- function(numbers, target) {
  
  finalList <- c(numbers)
  
  while(length(finalList) != target) {
    
    lastNum <- finalList[[length(finalList)]]
    
    there <- which(finalList == lastNum)
    
    if(length(there) == 1) {
      finalList <- c(finalList, 0)
    } else {
      lastSpoke <- there[[length(there)]]
      recent <- there[[(length(there)-1)]]
      newNum <- lastSpoke - recent
      finalList <- c(finalList, newNum)
    }
    
    if(length(finalList)%%10000 == 0) {
      print(paste('length of list at:', length(finalList)))
      print(lubridate::now())
    }
  }
  
  return(finalList)
}


# ex1 <- c(1,3,2)
# 
# temp <- tibble(results = part1(ex1, 2020),
#                results2 = part1(c(2,1,3), 2020))

input <- c(19,20,14,0,9,1)

#this runs pretty quick, but growing the vector will make a nightmare for second part
part1(input, 2020)

# this runs really, really slow, i am guessing it would take years to solve
part1Algo1<- part1(input, 30000000)

part2 <- function(numbers, target) {
  
  startingNumbers <- c(numbers)
  
  finalTibble <- tibble(number = numbers,
                        lastSpoke = c(1:length(numbers)),
                        recent = c(0))
  
  turn <- length(numbers) + 1
  
  while(max(finalTibble$lastSpoke) != target) {
    
    lastNum <- finalTibble %>% 
      filter(lastSpoke == max(lastSpoke)) %>% 
      pull(number)
    
    if(!(lastNum %in% finalTibble$number)) {
      # update the 0 position details
      finalTibble <- finalTibble %>% 
        mutate(recent = ifelse(number == 0,
                               lastSpoke, recent),
               lastSpoke = ifelse(number == 0,
                                  turn, lastSpoke))
    } else {
      
      selectedNum <- finalTibble %>% 
        filter(lastNum == number)
      
      if(selectedNum$recent == 0) {
        finalTibble <- finalTibble %>% 
          mutate(recent = ifelse(number == 0,
                                 lastSpoke, recent),
                 recent = ifelse(number == selectedNum$number,
                                 turn, recent),
                 lastSpoke = ifelse(number == 0,
                                    turn, lastSpoke)
                 )
      } else {
        
        newNum <- selectedNum$lastSpoke - selectedNum$recent
        
        if(newNum %in% finalTibble$number) {
          finalTibble <- finalTibble %>% 
            mutate(recent = ifelse(number == newNum,
                                   lastSpoke, recent),
                   lastSpoke = ifelse(number == newNum,
                                      turn, lastSpoke))
        } else {
          temp <- tibble(number = newNum, lastSpoke = turn,
                         recent = 0)
          
          finalTibble <- bind_rows(finalTibble, temp)
        } 
        
      }
      
    }
    
    if(turn%%10000 == 0) {
      print(paste('turnNumber at:', turn))
      print(lubridate::now())
    }
    
    turn <- turn + 1
    
    
  }

  targetNum <- finalTibble %>% 
    filter(lastSpoke == max(lastSpoke)) %>% 
    pull(number)
  return(targetNum)
}

part2(c(0,3,6), 2020)
part2(c(19,20,14,0,9,1),2020)

# the part 2 function will take ~70 ish hours to complete in linear time, so faster but still not great
temp2 <- part2(c(19,20,14,0,9,1),10000)

#ideas borrowed from tea and stats again (https://selbydavid.com/2020/12/06/advent-2020/#day14).  I thought i was building something like this in part2 but no where near this fast
memory_game3 <- function(n, start) {
  nstart <- length(start)
  spoken <- numeric(max(n, start) + 1)
  spoken[start[-nstart] + 1] <- seq_len(nstart - 1)
  current <- start[nstart]
  for (i in nstart:(n-1)) {
    next_number <- (spoken[current + 1] > 0) * i - spoken[current + 1]
    spoken[current + 1] <- i
    current <- next_number
  }
  current
}

memory_game3(3e7, c(19,20,14,0,9,1))
# I think the secret is the indexing system and building out the full length vector first

