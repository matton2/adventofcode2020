# trying day 8

library(tidyverse)

# part 1

boot <- read_lines('day8/day8.txt')

lineRead <- c(0)
acc <- 0
nextline <- 1

while(!any(duplicated(lineRead))) {
  
  
  print(lineRead)
  #print(nextline)
  print(acc)
  
  opp <- str_sub(boot[nextline], start = 1, end = 3)
  val <- as.numeric(str_sub(boot[nextline], start = 5))
  lineRead <- c(lineRead, nextline)
  
  
  if(opp == 'acc') {
    acc <- acc + val
    nextline <- nextline + 1
  } else if (opp == 'jmp') {
    nextline <- nextline + val
  } else if (opp == 'nop') {
    nextline <- nextline + 1
  }
  
}

boot <- read_lines('day8/day8Fix.txt')

lineRead <- c(0)
acc <- 0
nextline <- 1

while(!any(duplicated(lineRead))) {
  
  
  print(lineRead)
  #print(nextline)
  print(paste('acc at:', acc))
  
  opp <- str_sub(boot[nextline], start = 1, end = 3)
  val <- as.numeric(str_sub(boot[nextline], start = 5))
  lineRead <- c(lineRead, nextline)
  
  
  if(opp == 'acc') {
    acc <- acc + val
    nextline <- nextline + 1
  } else if (opp == 'jmp') {
    nextline <- nextline + val
  } else if (opp == 'nop') {
    nextline <- nextline + 1
  }
  
  print(paste('next line:', nextline))
  
  # my error was at line 346 (a jmp -> nop fixed it).  I had to brute force it a little.
  
}
