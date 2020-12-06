library(tidyverse)

task3 <- read_csv("day3/day3.csv", col_names = FALSE) %>% 
  select(X1)


# part 1

task3a <- task3 %>% 
  mutate(path = paste0(X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1),
         path = paste0(path, path, path, path), 
         pathPosition = lag((row_number() * 3) + 1),
         location = str_sub(path, start = pathPosition, end = pathPosition),
         hitTree = if_else(str_sub(path, start = pathPosition, end = pathPosition) == "#", TRUE, FALSE))


partA <- sum(task3a$hitTree, na.rm = TRUE)

# part 2

path5Temp <- c()
j <- 1
for(i in 1:NROW(task3)) {
  
  if(i != 1) {
    if(i %% 2 != 0) {
      j <- j+1
      path5Temp <- c(path5Temp, j)
    } else {
      path5Temp <- c(path5Temp, NA)
    }
  } else {
    path5Temp <- c(path5Temp, NA)
  }
  
}

task3b <- task3 %>% 
  mutate(path = paste0(X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1),
         path = paste0(path, path, path, path,path,path,path,path,path), 
         pathPosition1 = lag((row_number() * 1)  + 1),
         pathPosition2 = lag((row_number() * 3) + 1),
         pathPosition3 = lag((row_number() * 5) + 1),
         pathPosition4 = lag((row_number() * 7) + 1),
         pathPosition5 = path5Temp,
         hitTree1 = if_else(str_sub(path, start = pathPosition1, end = pathPosition1) == "#", TRUE, FALSE),
         hitTree2 = if_else(str_sub(path, start = pathPosition2, end = pathPosition2) == "#", TRUE, FALSE),
         hitTree3 = if_else(str_sub(path, start = pathPosition3, end = pathPosition3) == "#", TRUE, FALSE),
         hitTree4 = if_else(str_sub(path, start = pathPosition4, end = pathPosition4) == "#", TRUE, FALSE),
         hitTree5 = if_else(str_sub(path, start = pathPosition5, end = pathPosition5) == "#", TRUE, FALSE))


path1 <- sum(task3b$hitTree1, na.rm = TRUE)
path2 <- sum(task3b$hitTree2, na.rm = TRUE)
path3 <- sum(task3b$hitTree3, na.rm = TRUE)
path4 <- sum(task3b$hitTree4, na.rm = TRUE)
path5 <- sum(task3b$hitTree5, na.rm = TRUE)

partB <- path1*path2*path3*path4*path5
