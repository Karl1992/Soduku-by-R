# Backtracking
BT.soduku <- function(MyZ){
  Solution <- MyZ
  
  Blankindex <- mat.or.vec(0,2)
  
  candidate <- list()
  
  for (i in 1:9){
    for (j in 1:9){
      if (Solution[i,j] == 0){
        Blankindex <- rbind(Blankindex, c(i,j))
        can <- c(1:9)
        if(i %% 3 == 0){
          rlimit <- i
        }else{
          if(i %% 3 == 1){
            rlimit <- i + 2
          }else{
            rlimit <- i + 1
          }
        }
        
        if(j %% 3 == 0){
          climit <- j
        }else{
          if(j %% 3 == 1){
            climit <- j + 2
          }else{
            climit <- j + 1
          }
        }
        
        for(k in (rlimit - 2):rlimit){
          for(l in (climit - 2):climit){
            if(Solution[k,l] %in% can){
              can <- can[-which(can == Solution[k,l])]
            }
          }
        }
        
        for (k in 1:9){
          if (Solution[k,j] %in% can){
            can <- can[-which(can == Solution[k,j])]
          }
        }
        
        for (l in 1:9){
          if (Solution[i,l] %in% can){
            can <- can[-which(can == Solution[i,l])]
          }
        }
        candidate[[nrow(Blankindex)]] <- can
      }
    }
  }
  
  nofB <- nrow(Blankindex)
  
  numberofr <- 1
  
  iter <- 1
  
  Starttime <- as.numeric(Sys.time())
  
  while (numberofr <= nofB) {
    x <- Blankindex[numberofr,1]
    y <- Blankindex[numberofr,2]
    
    if (Solution[x,y] == 0){
    truecan <- candidate[[numberofr]]
    }else{
      if (which(candidate[[numberofr]] == Solution[x,y]) == length(candidate[[numberofr]])){
        Solution[x,y] <- 0
        numberofr <- numberofr - 1
        next
      }else{
      truecan <- candidate[[numberofr]][(which(candidate[[numberofr]] == Solution[x,y])+1):length(candidate[[numberofr]])]
      }
    }
    
    ncan <- length(truecan)
    
    index <- 1
    Solution[x,y] <- truecan[index]
    while (index <= ncan){
      if (countmistake(Solution,x,y) == 0){
        break
      }else{
        index <- index + 1
        Solution[x,y] <- truecan[index]
      }
    }
      
    if (index == ncan + 1){
      Solution[x,y] <- 0
      numberofr <- numberofr - 1
      x <- Blankindex[numberofr,1]
      y <- Blankindex[numberofr,2]
    }else{
      numberofr <- numberofr + 1
    }
    
    thistime <- round(as.numeric(Sys.time()) - Starttime, 2)
    png(filename = paste0("./1/gif", formatC(iter, 9), ".png"))
    SudokuPlot3(Solution, iter, thistime)
    dev.off()
    iter <- iter + 1
  }
}

MyZ<-read.table(file.choose())
MyZ<-matrix(unlist(MyZ), ncol = 9, byrow = TRUE)
dir.create("./1")
BT.soduku(MyZ)
frames2gif(pathIn = paste0("./", 1, "/"), pathOut = paste0("./Sudoku_simple-BT.gif"), ImageMagick_path= "D:/Games/ImageMagick-7.0.8-Q16/convert", delay = 3, everyFrame = 1000)
unlink("./1", recursive=TRUE, force=TRUE)
