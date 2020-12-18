library(tidyverse)


example <- "1 + 2 * 3 + 4 * 5 + 6"

example2 <- "1 + (2 * 3) + (4 * (5 + 6))"

example3 <- "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

eval(parse(text = example3))

weirdMath <- function(mathString) {
  
  parsedString <- str_split(mathString, " ")[[1]]
  
  tempNum <- 0
  
  tempNum <- eval(parse(text = paste(parsedString[[1]],
                                     parsedString[[2]],
                                     parsedString[[3]],
                                     collapse = "")))
  
  if(length(parsedString) > 3) {
    for(i in seq(4,length(parsedString),2)) {
      tempNum <- 
        eval(parse(text = paste(tempNum,
                                parsedString[[i]],
                                parsedString[[i+1]],
                                collapse = "")))
    }
  }
  
  return(tempNum)
  
  
}

weirdMath(example)

# i cant figure out how to get the para thing to operate.  I was thinking of creating my own AST but i am not sure i know how to do that....
paraMath <- function(fullMathString) {
  
  someNumber <- 0
  # are there any parathensis
  
  parsedString <- str_split(fullMathString, " ")[[1]]
  
  for(i in 1:length(parsedString)) {
    
    
    
  }
  
  if(any(str_detect(fullMathString, "\\("))) {
    #operate inside the paras first, and then add that to the rest
    
    if(str_count(fullMathString, "\\(") == 1) {
      
      open <- str_locate(fullMathString, "\\(")[[1]]
      closed <- str_locate(fullMathString, "\\)")[[1]]
      
      first <- str_sub(fullMathString, (open+1), (closed-1))
      
      
    }
    
  } else {
    someNumber <- weirdMath(fullMathString)
  }
  
  
  return(someNumber)
  
}

paraMath(example)
