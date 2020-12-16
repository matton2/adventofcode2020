library(tidyverse)

# taking on task 1

task1 <- read_csv("day1/task1.csv") %>% 
  select (X2)

for (i in 1:NROW(task1)) {
  needed <- 2020 - task1$X2[[i]]
  
  for(j in i:NROW(task1)) {
    if (task1$X2[[j]] == needed) {
      print(task1$X2[[i]])
      print(task1$X2[[j]])
      print(task1$X2[[i]]*task1$X2[[j]])
      
      break()
    }
  }
  
}

# taking on task 2

for (i in 1:NROW(task1)) {
  needed1 <- 2020 - task1$X2[[i]]
  
  for(j in i:NROW(task1)) {
    needed2 <- needed1 - task1$X2[[j]]
    for(k in j:NROW(task1)) {
      if (task1$X2[[k]] == needed2) {
        print(task1$X2[[i]])
        print(task1$X2[[j]])
        print(task1$X2[[k]])
        print(task1$X2[[i]]*task1$X2[[j]]*task1$X2[[k]])
    }
    }
  }
  
}
