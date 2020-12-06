library(tidyverse)

task4 <- read_csv("day4/task4_a.csv", col_names = FALSE) %>% 
  select(X1) 

passportCount <- 1
passport <- c()

for (i in 1:NROW(task4)) {
  
  if (!is.na(task4$X1[[i]])) {
    passport <- c(passport, passportCount)
  } else {
    passport <- c(passport, NA)
    passportCount <- passportCount + 1
  }
  
}

task4Update <- task4 %>% 
  mutate(passportNumber = passport) %>% 
  filter(!is.na(X1)) %>% 
  group_by(passportNumber) %>% 
  summarise(text = paste(X1, collapse = " ")) %>% 
  mutate(fieldCount = str_count(text, ":"),
         valid = if_else(fieldCount == 8 | (fieldCount == 7 & !str_detect(text, 'cid')), TRUE, FALSE))


valid <- sum(task4Update$valid)

# going after task 2

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

checkIfValid <- function(str, value) {
  
  if(str == 'byr') {
    value <- as.numeric(value)
    if(1920 <= value & value <= 2002) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(str == 'iyr') {
    value <- as.numeric(value)
    if(2010 <= value & value <= 2020) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(str == 'eyr') {
    value <- as.numeric(value)
    if(2020 <= value & value <= 2030) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(str == "hgt") {
    unit <- paste(str_extract_all(value, "[:alpha:]", simplify = TRUE),collapse = "")
    
    if(is.na(unit)) {
      return(FALSE)
    }
    
    value <- paste(str_extract_all(value, "[:digit:]", simplify = TRUE),collapse = "")
    if(unit == "cm") {
      if(150 <= value & value <= 193) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (unit == 'in') {
      if(59 <= value & value <= 76) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
    
  }
  
  if(str == "hcl") {
    if(str_sub(value, 1, 1) != "#") {
      return(FALSE)
    }
    
    value <- str_remove(value, "#") 
    
    if(str_count(value) != 6) {
      return(FALSE)
    }
    
    if(any(str_detect(value, letters[7:26]))) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  if(str == "ecl") {
    
    if(value %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(str == "pid") {
    if(str_count(value) == 9) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(str == 'cid') {
    return(TRUE)
  }
  
}

passedChecks <- c()

for(i in 1:NROW(task4Update)) {
  temp <- str_split(task4Update$text[[i]], pattern = " ")
  valid <- c()
  for(j in 1:length(temp[[1]])) {
    temp2 <- str_split(temp[[1]][[j]], ":")
    res <- checkIfValid(temp2[[1]][[1]], temp2[[1]][[2]])
    valid <- c(valid, res)
    if(res == FALSE) {break}
  }
  
  final <- all(valid)
  
  passedChecks <- c(passedChecks, final)
  
  
}


task4b <- task4Update %>% 
  mutate(passed = passedChecks,
         allGood = if_else(valid == TRUE & passed == TRUE, TRUE, FALSE))


sum(task4b$allGood)
