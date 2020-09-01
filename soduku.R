library("tcltk")
library("tkrplot")
library("Matrix")

z<-mat.or.vec(9,9)

SudokuPlot <- function(z) {
  cols <- ifelse(z, "blue","black")
  par(mar=c(1,1,1,1), bg="white")
  plot(0.5:9.5, 0.5:9.5, type="n", axes=FALSE, xlab="", ylab="")
  cusr <<- par("usr"); cplt <<- par("plt")
  segments(0.5:9.5, rep(0.5,10), 0.5:9.5, rep(9.5,10), col="grey")
  segments(rep(0.5,10), 0.5:9.5, rep(9.5,10), 0.5:9.5, col="grey")
  segments(c(0,3,6,9)+0.5, rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), lwd=3)
  segments(rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), c(0,3,6,9)+0.5, lwd=3)
  for (i in 1:9) for (j in 1:9) if (z[i,j]) {
    if (cols[i,j]=="red") text(j, 10-i, "X", col="pink", cex=3)
    text(j, 10-i, z[i,j], col=cols[i,j], font=ifelse(cols[i,j]=="blue",2,1),
         cex=ifelse(cols[i,j]=="blue", 2.0, 1.8))
  }
}

SudokuPlot2 <- function(z, Newallmistake) {
  cols <- ifelse(z, "blue","black")
  par(mar=c(1,1,1,1), bg="white")
  plot(0.5:9.5, 0.5:9.5, type="n", axes=FALSE, xlab="", ylab="", main = paste("conflicts =", Newallmistake))
  cusr <<- par("usr"); cplt <<- par("plt")
  segments(0.5:9.5, rep(0.5,10), 0.5:9.5, rep(9.5,10), col="grey")
  segments(rep(0.5,10), 0.5:9.5, rep(9.5,10), 0.5:9.5, col="grey")
  segments(c(0,3,6,9)+0.5, rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), lwd=3)
  segments(rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), c(0,3,6,9)+0.5, lwd=3)
  for (i in 1:9) for (j in 1:9) if (z[i,j]) {
    if (cols[i,j]=="red") text(j, 10-i, "X", col="pink", cex=3)
    text(j, 10-i, z[i,j], col=cols[i,j], font=ifelse(cols[i,j]=="blue",2,1),
         cex=ifelse(cols[i,j]=="blue", 2.0, 1.8))
  }
}

SudokuPlot3 <- function(z, iter, thistime) {
  cols <- ifelse(z, "blue","black")
  par(mar=c(3,3,3,3), bg="white")
  plot(0.5:9.5, 0.5:9.5, type="n", axes=FALSE, xlab="", ylab="", main = c(paste("Iterations =", iter), paste(thistime, "s")))
  cusr <<- par("usr"); cplt <<- par("plt")
  segments(0.5:9.5, rep(0.5,10), 0.5:9.5, rep(9.5,10), col="grey")
  segments(rep(0.5,10), 0.5:9.5, rep(9.5,10), 0.5:9.5, col="grey")
  segments(c(0,3,6,9)+0.5, rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), lwd=3)
  segments(rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), c(0,3,6,9)+0.5, lwd=3)
  for (i in 1:9) for (j in 1:9) if (z[i,j]) {
    if (cols[i,j]=="red") text(j, 10-i, "X", col="pink", cex=3)
    text(j, 10-i, z[i,j], col=cols[i,j], font=ifelse(cols[i,j]=="blue",2,1),
         cex=ifelse(cols[i,j]=="blue", 2.0, 1.8))
  }
}

# Here's how I read a sudoku, convert it to a matrix
# (the function above doesn't work on lists) and then plot it
setwd("D:/study/MasterinUSC/pm520/final project/soduku")
MyZ<-read.table(file.choose())
MyZ<-matrix(unlist(MyZ), ncol = 9, byrow = TRUE)

png(filename = "simple.png")
SudokuPlot(MyZ)
dev.off()

set.seed(20190423)

# Starting_Temp <- 10
# Tempdecreaserate <- 0.001
# Final_Temp <- 0.00001
# Temp <- Starting_Temp
# OuterLOOPN <- 20
# InnerLOOPN <- 2209
# N_I <- 1
# N_O <- 1
# q <- 0.8
SA.soduku <- function(Starting.Temp, Tempdecreaserate, Final.Temp, countmistake, GenerateNewS, MyZ, SudokuPlot, reptime){
  
  dir.create(paste0("./", reptime))
  Temp <- Starting.Temp
  
  Solution <- MyZ
  
  Blank <- mat.or.vec(9,9)
  
  for(i in 1:9){
    for(j in 1:9){
      if(Solution[i,j] == 0){
        Blank[i,j] = 1
      }
    }
  }
  
  for (i in 1:3) {
    for (j in 1:3){
      standard <- c(1:9)
      Exist <- numeric()
      for (k in (i*3-2):(i*3)){
        for (l in (j*3-2):(j*3)){
          if (Blank[k,l] != 1) {
            Exist <- c(Exist, which(standard == Solution[k,l]))
          }
        }
      }
      candidate <- standard[-Exist]
      for (k in (i*3-2):(i*3)){
        for (l in (j*3-2):(j*3)){
          if (Blank[k,l] == 1) {
            if (length(candidate) != 1){
              Solution[k,l] <- sample(candidate,1)
              candidate <- candidate[-which(candidate == Solution[k,l])]
            }else{
              Solution[k,l] <- candidate
            }
          }
        }
      }
      
    }
  }
  
  allmistake <- countmistake(Solution)
  
  iter <- 1
  
  mistakerec <- allmistake
  
  while (Temp > Final.Temp){
    
      if(allmistake == 0){
        break
      }
      
      Newsolution <- GenerateNewS(Solution, Blank)
      
      Newallmistake <- countmistake(Newsolution)
      
      p <- runif(1)
      
      h <- min(1, exp(-1*(Newallmistake-allmistake)/Temp)) #* q)
      
      if(p < h){
        Solution <- Newsolution
        allmistake <- Newallmistake
        png(filename = paste0("./", reptime, "/gif", formatC(iter, 5), ".png"))
        SudokuPlot2(Solution, Newallmistake)
        dev.off()
        
        mistakerec <-c(mistakerec, Newallmistake)
        
        iter <- iter + 1
      }
      
      Temp <- Temp*(1 - Tempdecreaserate)
    # N_I <- N_I + 1
    # }
    # 
    # N_O <- N_O + 1
    # 
    # q <- q*0.8
  }
    
  SudokuPlot2(Solution, Newallmistake)
  
  return(mistakerec)
}

alltime <- numeric()

for (i in 1: 1){
starttime <- Sys.time()

mistakerec <- SA.soduku(10,0.00005,0.0001,countmistake,GenerateNewS,MyZ, SudokuPlot, i)

thistime <- ceiling(Sys.time() - starttime)

alltime <- c(alltime, as.numeric(thistime))

frames2gif(pathIn = paste0("./", i, "/"), pathOut = paste0("./testsudoku",i , ".gif"), ImageMagick_path= "D:/Games/ImageMagick-7.0.8-Q16/convert", delay = 2, everyFrame = 20)

unlink(paste0("./", i), recursive=TRUE, force=TRUE)

par(mar = c(3,3,3,3))

png(filename = paste0("./", "/convergence", i, ".png"))
plot(mistakerec, xlab = "Iterations", ylab = "Mistake", main = c("Convergence", paste("time =", alltime,"min")), type = "l", ylim = c(0, 50))
dev.off()
}

png(filename = "testSudoku.png")
plot(alltime, xlab = "Repeat time", ylab = "Time", xaxt = "n", main = paste("average time =", mean(alltime), "s"), type = "b")
abline(h = mean(alltime), col = "red")
axis(1, c(1:10))
dev.off()

