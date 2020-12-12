library(tidyverse)
library(R6)


boat <- R6Class('boat',
    public = list(
      
      facing = NULL,
      ew = NULL,
      ewPosition = NULL,
      ns = NULL,
      nsPosition = NULL,
      initialize = function(facing) {
        self$facing <- facing
        self$ew <- 'E'
        self$ewPosition <- 0
        self$ns <- "N"
        self$nsPosition <- 0
      },
      checkForOpposite = function(facing, direction) {
        
        #if facing and direction the same, return false
        if(facing == direction) {
          return(FALSE)
        }
        
        if(facing == 'E' & direction == "W") {
          return(TRUE)
        } else if(facing == "W" & direction == "E") {
          return(TRUE)
        }
        
        if(facing == 'N' & direction == "S") {
          return(TRUE)
        } else if(facing == "S" & direction == "N") {
          return(TRUE)
        }
      
      },
      
      turnTheBoat = function(facing, turn, degrees) {
        
        newFacing <- NA
        
        if(facing == 'E') {
          if(turn == 'L') {
            if(degrees == 90) {
              newFacing <- "N"
            } else if(degrees == 180) {
              newFacing <- "W"
            } else if(degrees == 270) {
              newFacing <- "S"
            }
          } else if(turn == "R") {
            if(degrees == 90) {
              newFacing <- "S"
            } else if(degrees == 180) {
              newFacing <- "W"
            } else if(degrees == 270) {
              newFacing <- "N"
            }
          }
        }
        
        if(facing == 'W') {
          if(turn == 'R') {
            if(degrees == 90) {
              newFacing <- "N"
            } else if(degrees == 180) {
              newFacing <- "E"
            } else if(degrees == 270) {
              newFacing <- "S"
            }
          } else if(turn == "L") {
            if(degrees == 90) {
              newFacing <- "S"
            } else if(degrees == 180) {
              newFacing <- "E"
            } else if(degrees == 270) {
              newFacing <- "N"
            }
          }
        }
        
        if(facing == 'N') {
          if(turn == 'R') {
            if(degrees == 90) {
              newFacing <- "E"
            } else if(degrees == 180) {
              newFacing <- "S"
            } else if(degrees == 270) {
              newFacing <- "W"
            }
          } else if(turn == "L") {
            if(degrees == 90) {
              newFacing <- "W"
            } else if(degrees == 180) {
              newFacing <- "S"
            } else if(degrees == 270) {
              newFacing <- "E"
            }
          }
        }
        
        if(facing == 'S') {
          if(turn == 'L') {
            if(degrees == 90) {
              newFacing <- "E"
            } else if(degrees == 180) {
              newFacing <- "N"
            } else if(degrees == 270) {
              newFacing <- "W"
            }
          } else if(turn == "R") {
            if(degrees == 90) {
              newFacing <- "W"
            } else if(degrees == 180) {
              newFacing <- "N"
            } else if(degrees == 270) {
              newFacing <- "E"
            }
          }
        }
        
        return(newFacing)
      
      },
      
      # moving that boat
      movingBoat = function(direction, number) {
        
        
        if(direction == "L" || direction == "R") {
          self$facing <- self$turnTheBoat(self$facing, direction, number)
        }
        
        
        if(direction == "F") {
          
          if(self$facing == "E") {
            if(self$ew == "E") {
              self$ewPosition <- self$ewPosition + number
            } else if(self$ew == "W") {
              newPosition <- self$ewPosition - number
              if(newPosition < 0) {
                self$ew <- "E"
                self$ewPosition <- abs(newPosition)
              } else {
                self$ewPosition <- newPosition
              }
            }
          }
          
          if(self$facing == "W") {
            if(self$ew == "W") {
              self$ewPosition <- self$ewPosition + number
            } else if(self$ew == "E") {
              newPosition <- self$ewPosition - number
              if(newPosition < 0) {
                self$ew <- "W"
                self$ewPosition <- abs(newPosition)
              } else {
                self$ewPosition <- newPosition
              }
            }
          }
          
          if(self$facing == "N") {
            if(self$ns == "N") {
              self$nsPosition <- self$nsPosition + number
            } else if(self$ns == "S") {
              newPosition <- self$nsPosition - number
              if(newPosition < 0) {
                self$ns <- "N"
                self$nsPosition <- abs(newPosition)
              } else {
                self$nsPosition <- newPosition
              }
            }
          }
          
          if(self$facing == "S") {
            if(self$ns == "S") {
              self$nsPosition <- self$nsPosition + number
            } else if(self$ns == "N") {
              newPosition <- self$nsPosition - number
              if(newPosition < 0) {
                self$ns <- "S"
                self$nsPosition <- abs(newPosition)
              } else {
                self$nsPosition <- newPosition
              }
            }
          }
          
          
          
        }
        
        if(direction == "N") {
          
          if(self$ns == "N") {
            self$nsPosition <- self$nsPosition + number
          } else if(self$ns == "S") {
            newPosition <- self$nsPosition - number
            if(newPosition < 0) {
              self$ns <- "N"
              self$nsPosition <- abs(newPosition)
            } else {
              self$nsPosition <- newPosition
            }
          }
          
          
        } else if(direction == "S") {
          
          if(self$ns == "S") {
            self$nsPosition <- self$nsPosition + number
          } else if(self$ns == "N") {
            newPosition <- self$nsPosition - number
            if(newPosition < 0) {
              self$ns <- "S"
              self$nsPosition <- abs(newPosition)
            } else {
              self$nsPosition <- newPosition
            }
          }
          
          
        } else if(direction == "W") {
          
          if(self$ew == "W") {
            self$ewPosition <- self$ewPosition + number
          } else if(self$ew == "E") {
            newPosition <- self$ewPosition - number
            if(newPosition < 0) {
              self$ew <- "W"
              self$ewPosition <- abs(newPosition)
            } else {
              self$ewPosition <- newPosition
            }
          }
          
          
        } else if(direction == "E") {
          if(self$ew == "E") {
            self$ewPosition <- self$ewPosition + number
          } else if(self$ew == "W") {
            newPosition <- self$ewPosition - number
            if(newPosition < 0) {
              self$ew <- "E"
              self$ewPosition <- abs(newPosition)
            } else {
              self$ewPosition <- newPosition
            }
          }
        }
        
        
      },
      
      manhattenDist = function() {
        self$ewPosition + self$nsPosition
      }
      
      
    )
)


day12ex <- read_lines('day12/day12ex.txt')

exBoat <- boat$new("E")

for(i in 1:length(day12ex)) {
  
  exBoat$movingBoat(str_extract(day12ex[[i]], "[:alpha:]"),
                    as.numeric(paste(str_extract_all(day12ex[[i]], "[:digit:]")[[1]],collapse = "")))
  
}

exBoat$manhattenDist()
exBoat$facing
exBoat$ewPosition
exBoat$nsPosition

day12 <- read_lines('day12/day12.txt')

problemBoat <- boat$new("E")

for(i in 1:length(day12)) {
  
  problemBoat$movingBoat(str_extract(day12[[i]], "[:alpha:]"),
                    as.numeric(paste(str_extract_all(day12[[i]], "[:digit:]")[[1]],collapse = "")))
  
  print(paste(i, problemBoat$facing, 
              problemBoat$ew, problemBoat$ewPosition,
              problemBoat$ns,
              problemBoat$nsPosition,
              problemBoat$manhattenDist()))
  # print(problemBoat$facing)
  # print(problemBoat$ew)
  # print(problemBoat$ewPosition)
  
}

problemBoat$manhattenDist()
problemBoat$ew
problemBoat$ewPosition
problemBoat$facing
problemBoat$ns
problemBoat$nsPosition


## part 2----

boatWayPoint <- R6Class('boat',
                public = list(
                  
                  ewPositionBoat = 0,
                  ewBoat = 'E',
                  nsPositionBoat = 0,
                  nsBoat = 'N',
                  ewPositionWay = NULL,
                  ewWay = NULL,
                  nsPositionWay = NULL,
                  nsWay = NULL,

                  initialize = function(ewPositionWay, nsPositionWay) {
                    self$ewPositionWay <- ewPositionWay
                    self$nsPositionWay <- nsPositionWay
                    self$ewWay <- "E"
                    self$nsWay <- "N"
                  
                  },

                  
                  turnTheWay = function(turn, degrees) {
                    
                    directionsClock <- c("N", "E", "S", "W", "N", "E", "S", "W")
                    directionCounter <- c("N", "W", "S", "E", "N", "W", "S", "E")
                    
                    if(turn == "L") {
                      if(degrees == 90) {
                        posEW <- str_which(directionCounter, self$ewWay)[[1]]
                        posNS <- str_which(directionCounter, self$nsWay)[[1]]
                        
                        self$ewWay <- directionCounter[[(posNS + 1)]]
                        self$nsWay<- directionCounter[[(posEW + 1)]]
                        
                        nsPos <- self$nsPositionWay
                        ewPos <- self$ewPositionWay
                        
                        self$ewPositionWay <- nsPos
                        self$nsPositionWay <- ewPos
                        
                      } else if(degrees == 180) {
                        
                        posEW <- str_which(directionCounter, self$ewWay)[[1]]
                        posNS <- str_which(directionCounter, self$nsWay)[[1]]
                        
                        self$ewWay <- directionCounter[[(posEW + 2)]]
                        self$nsWay <- directionCounter[[(posNS + 2)]]
                        
                        # self$ewPositionWay <- self$nsPositionWay
                        # self$nsPositionWay <- self$ewPositionWay
                        
                      } else if(degrees == 270) {
                        
                        posEW <- str_which(directionCounter, self$ewWay)[[1]]
                        posNS <- str_which(directionCounter, self$nsWay)[[1]]
                        
                        self$ewWay <- directionCounter[[(posNS + 3)]]
                        self$nsWay <- directionCounter[[(posEW + 3)]]
                        
                        nsPos <- self$nsPositionWay
                        ewPos <- self$ewPositionWay
                        
                        self$ewPositionWay <- nsPos
                        self$nsPositionWay <- ewPos
                        
                      }
                      
                    }
                    
                    if(turn == "R") {
                      if(degrees == 90) {
                        
                        posEW <- str_which(directionsClock, self$ewWay)[[1]]
                        posNS <- str_which(directionsClock, self$nsWay)[[1]]
                        
                        self$ewWay <- directionsClock[[(posNS + 1)]]
                        self$nsWay <- directionsClock[[(posEW + 1)]]
                        
                        nsPos <- self$nsPositionWay
                        ewPos <- self$ewPositionWay
                        
                        self$ewPositionWay <- nsPos
                        self$nsPositionWay <- ewPos

  
                      } else if(degrees == 180) {
                        
                        posEW <- str_which(directionsClock, self$ewWay)[[1]]
                        posNS <- str_which(directionsClock, self$nsWay)[[1]]
                        
                        self$ewWay <- directionsClock[[(posEW + 2)]]
                        self$nsWay <- directionsClock[[(posNS + 2)]]
                        
                        # self$ewPositionWay <- self$nsPositionWay
                        # self$nsPositionWay <- self$ewPositionWay
                        
                        
                      } else if(degrees == 270) {
                        
                        posEW <- str_which(directionsClock, self$ewWay)[[1]]
                        posNS <- str_which(directionsClock, self$nsWay)[[1]]
                        
                        self$ewWay <- directionsClock[[(posNS + 3)]]
                        self$nsWay <- directionsClock[[(posEW + 3)]]
                        
                        nsPos <- self$nsPositionWay
                        ewPos <- self$ewPositionWay
                        
                        self$ewPositionWay <- nsPos
                        self$nsPositionWay <- ewPos
                        
                        
                      }
                    }
                    
                  },
                  
                  # moving that boat
                  movingBoat = function(direction, number) {
                    
                    
                    if(direction == "L" || direction == "R") {
                      self$turnTheWay(direction, number)
                    }
                    
                    # move the boat based on way
                    if(direction == "F") {
                      
                      if(self$ewWay == self$ewBoat) {
                        self$ewPositionBoat <- self$ewPositionBoat + (number*self$ewPositionWay)
                      } else {
                        newPos <- self$ewPositionBoat - (number*self$ewPositionWay)
                        if(newPos < 0) {
                          if(self$ewBoat == "E") {
                            self$ewBoat <- "W"
                            self$ewPositionBoat <- abs(newPos)
                          } else if(self$ewBoat == "W") {
                            self$ewBoat <- "E"
                            self$ewPositionBoat <- abs(newPos)
                          }
                        } else {
                          self$ewPositionBoat <- newPos
                        }
                      }
                      
                      if(self$nsWay == self$nsBoat) {
                        self$nsPositionBoat <- self$nsPositionBoat + (number*self$nsPositionWay)
                      } else {
                        newPos <- self$nsPositionBoat - (number*self$nsPositionWay)
                        if(newPos < 0) {
                          if(self$nsBoat == "N") {
                            self$nsBoat <- "S"
                            self$nsPositionBoat <- abs(newPos)
                          } else if(self$nsBoat == "S") {
                            self$nsBoat <- "N"
                            self$nsPositionBoat <- abs(newPos)
                          }
                        } else {
                          self$nsPositionBoat <- newPos
                        }
                      }
                    }
                    
                    if(direction == "N") {
                      
                      if(self$nsWay == "N") {
                        self$nsPositionWay <- self$nsPositionWay + number
                      } else if(self$nsWay == "S") {
                        newPosition <- self$nsPositionWay - number
                        if(newPosition < 0) {
                          self$nsWay <- "N"
                          self$nsPositionWay <- abs(newPosition)
                        } else {
                          self$nsPositionWay <- newPosition
                        }
                      }
                      
                      
                    } else if(direction == "S") {
                      
                      if(self$nsWay == "S") {
                        self$nsPositionWay <- self$nsPositionWay + number
                      } else if(self$nsWay == "N") {
                        newPosition <- self$nsPositionWay - number
                        if(newPosition < 0) {
                          self$nsWay <- "S"
                          self$nsPositionWay <- abs(newPosition)
                        } else {
                          self$nsPositionWay <- newPosition
                        }
                      }
                      
                      
                    } else if(direction == "W") {
                      
                      if(self$ewWay == "W") {
                        self$ewPositionWay <- self$ewPositionWay + number
                      } else if(self$ewWay == "E") {
                        newPosition <- self$ewPositionWay - number
                        if(newPosition < 0) {
                          self$ewWay <- "W"
                          self$ewPositionWay <- abs(newPosition)
                        } else {
                          self$ewPositionWay <- newPosition
                        }
                      }
                      
                      
                    } else if(direction == "E") {
                      if(self$ewWay == "E") {
                        self$ewPositionWay <- self$ewPositionWay + number
                      } else if(self$ewWay == "W") {
                        newPosition <- self$ewPositionWay - number
                        if(newPosition < 0) {
                          self$ewWay <- "E"
                          self$ewPositionWay <- abs(newPosition)
                        } else {
                          self$ewPositionWay <- newPosition
                        }
                      }
                    }
                    
                    
                  },
                  
                  manhattenDist = function() {
                    self$ewPositionBoat + self$nsPositionBoat
                  }
                  
                  
                )
)

# partBoatEx <- boatWayPoint$new(10,1)
# 
# for(i in 1:length(day12ex)) {
#   partBoatEx$movingBoat(str_extract(day12ex[[i]], "[:alpha:]"),
#                     as.numeric(paste(str_extract_all(day12ex[[i]], "[:digit:]")[[1]],collapse = "")))
#   
#   print(paste(i,
#               partBoatEx$ewWay, 
#               partBoatEx$ewPositionWay,
#               partBoatEx$nsWay,
#               partBoatEx$nsPositionWay,
#               partBoatEx$manhattenDist()
#         ))
# }

problemBoat2 <- boatWayPoint$new(10,1)

for(i in 1:length(day12)) {
  
  problemBoat2$movingBoat(str_extract(day12[[i]], "[:alpha:]"),
                         as.numeric(paste(str_extract_all(day12[[i]], "[:digit:]")[[1]],collapse = "")))
  
  print(paste(i, 
              problemBoat2$ewWay, 
              problemBoat2$ewPositionWay,
              problemBoat2$nsWay,
              problemBoat2$nsPositionWay,
              problemBoat2$manhattenDist()))
  # print(problemBoat$facing)
  # print(problemBoat$ew)
  # print(problemBoat$ewPosition)
  
}

problemBoat2$manhattenDist()
