# Version for Backtracking
countmistake <- function(Solution,x,y){
  
  count <- 0
  
  if(x %% 3 == 0){
    rlimit <- x
  }else{
    if(x %% 3 == 1){
      rlimit <- x + 2
    }else{
      rlimit <- x + 1
    }
  }
  
  if(y %% 3 == 0){
    climit <- y
  }else{
    if(y %% 3 == 1){
      climit <- y + 2
    }else{
      climit <- y + 1
    }
  }
  
  for(k in (rlimit - 2):rlimit){
    for(l in (climit - 2):climit){
      if(Solution[x,y] == Solution[k,l]){
        count <- count + 1
      }
    }
  }
  

  for(l in 1:9){
    count <- count + as.numeric(Solution[x,l] == Solution[x,y])
  }
  
  for(k in 1:9){
    count <- count + as.numeric(Solution[k,y] == Solution[x,y])
  }
  
  count <- count - 3
  
  return(count)
}

# Version 2
countmistake <- function(M){
  count <- 0
  for(i in 1:9){
    row_r <- table(M[i,])
    count <- count + sum(row_r - 1)
  }
  for(j in 1:9){
    col_r <- table(M[,j])
    count <- count + sum(col_r - 1)
  }
  return(count)
}


