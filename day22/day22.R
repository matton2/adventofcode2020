library(tidyverse)

figureOutWhoWins <- function(player1, player2) {
  
  player1 <- player1
  player2 <- player2
  
  while(length(player1) != 0 || length(player2) != 0) {
    
    player1Card <- player1[1]
    player2Card <- player2[1]
    
    player1 <- player1[-1]
    player2 <- player2[-1]
    
    if(player1Card > player2Card) {
      
      player1 <- c(player1, player1Card, player2Card)
      
    } else {
      
      player2 <- c(player2, player2Card, player1Card)
      
    }
    
    if(length(player1) == 0) {
      
      temp <- seq(from = length(player2), to = 1, by = -1)
      
      return(sum(player2*temp))
      
    }
    
    if(length(player2) == 0) {
      temp <- seq(from = length(player1), to = 1, by = -1)
      
      return(sum(player1*temp))
    }
    
  }
  

  
}

player1ex <- read_lines('day22/day22ex1.txt', skip = 1, n_max = 5) %>% as.numeric()

player2ex <- read_lines('day22/day22ex1.txt', skip = 8) %>% as.numeric()

figureOutWhoWins(player1ex, player2ex)

player1 <- read_lines('day22/day22.txt', skip = 1, n_max = 25) %>% as.numeric()

player2 <- read_lines('day22/day22.txt', skip = 28) %>% as.numeric()

figureOutWhoWins(player1, player2)
