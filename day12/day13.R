library(tidyverse)

day13 <- read_lines('day13/day13.txt')

target <- as.numeric(day13[[1]])
buses <- str_split(day13[[2]], ",")[[1]]
times <- c()

for(i in 1:length(buses)) {
  if(str_detect(buses[[i]], '[:digit:]')) {
    temp <- as.numeric(buses[[i]])
    times <- c(times, temp)
  }
}

for(i in 1:length(times)) {
  
  temp <- seq(0, target+times[[i]], by = times[[i]])
  
  tempLength <- length(temp)
  
  diff <- target-temp[[tempLength]]
  
  print(paste('Bus:', times[[i]], "Time Difference:", diff))
  
  
}

# part2

# example stuff
exDepart <- c(0, 1, 2, 3)
exBusTimes <- c(1789,37,47,1889)

tictoc::tic()
foundIt <- TRUE
i <- 1192161486
while(foundIt) {
  
  firstTime <- (i) %% exBusTimes[[1]]
  secondTime <- (i + exDepart[[2]]) %% exBusTimes[[2]]
  thridTime <- (i + exDepart[[3]]) %% exBusTimes[[3]]
  fourthTime <- (i + exDepart[[4]]) %% exBusTimes[[4]]
  # fifthTime <- (i + exDepart[[5]]) %% exBusTimes[[5]]
  
  
  values <- c(firstTime, secondTime, thridTime,fourthTime)
  
  if(all(values == 0)) {
    foundIt <- FALSE
    print(paste('found it:', i))
  }
  
  if(i%%100000==0) {
    print(paste("i:", i))
  } 
  
  i <- i + 1
  
}
tictoc::toc()

print(numbers::chinese(departTimes, times), digits = 22)

# the above code runs fine and finds all the correct answers

departTimes <- c()

for(i in 1:length(buses)) {
  if(str_detect(buses[[i]], '[:digit:]')) {
    departTimes <- c(departTimes, (i-1))
  }
}

# so the time series i am looking for is seperated by depart times


departTimes
times

times + departTimes


tictoc::tic()
foundIt <- TRUE
#i <- 100004272771165
i <- 415579909629970
while(foundIt) {
  
  firstTime <- (i) %% times[[1]]
  secondTime <- (i + departTimes[[2]]) %% times[[2]]
  thridTime <- (i + departTimes[[3]]) %% times[[3]]
  fourthTime <- (i + departTimes[[4]]) %% times[[4]]
  fifthTime <- (i + departTimes[[5]]) %% times[[5]]
  sixthTime <- (i + departTimes[[6]]) %% times[[6]]
  seventhTime <- (i + departTimes[[7]]) %% times[[7]]
  eithTime <- (i + departTimes[[8]]) %% times[[8]]
  ninthTime <- (i + departTimes[[9]]) %% times[[9]]
  
  values <- c(firstTime, secondTime, thridTime, fourthTime, fifthTime,
              sixthTime, seventhTime, eithTime, ninthTime)
  
  if(all(values == 0)) {
    foundIt <- FALSE
    print(paste('found it:', i))
  }
  
  if(i%%1000000==0) {
    print(paste("i:", i))
  }
  
  i <- i + 1
}
tictoc::toc()





# code from tea and stats (https://selbydavid.com/2020/12/06/advent-2020/)

# this uses the chinese remainder theory which i have never heard of before

input <- read_lines('day13/day13.txt')
timestamp <- as.integer(input[1])
buses <- as.integer(strsplit(input[2], ',')[[1]])

inservice <- buses[!is.na(buses)]
inservice[which.min(-timestamp %% inservice)] * min(-timestamp %% inservice)

sieve <- function(a1, a2, n1, n2, maxit = 1e5) {
  x <- a1 + n1 * (0:maxit)
  x[which.max(x %% n2 == a2 %% n2)]
}

find_timetable2 <- function(buses) {
  offsets <- -(seq_along(buses) - 1)[!is.na(buses)] # a
  buses <- buses[!is.na(buses)]                     # n
  x <- offsets[1]
  for (i in 2:length(buses))
    x <- sieve(x, offsets[i], prod(head(buses, i-1)), buses[i])
  x
}

format(find_timetable2(buses), sci = FALSE)
