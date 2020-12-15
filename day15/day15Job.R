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

# part1(input, 2020)
# tictoc::tic()
part2Alg1 <- part1(input, 30000000)
#tictoc::toc()