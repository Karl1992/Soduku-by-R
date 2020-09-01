# # version 1.1
# 
# for (i in 1:9) {
#   Exist <- Solution[i,-which(Blank[i,] == 1)]
#   standard <- c(1:9)
#   for(j in standard){
#     if(j %in% Exist){
#       standard <- standard[-which(standard == j)]
#     }
#   }
#   if(length(standard == 1)){
#     Solution[i,which(Blank[i,] == 1)] <- standard
#   }else{
#     Solution[i,which(Blank[i,] == 1)] <- sample(c(standard),sum(Blank[i,]))
#   }
# }
# 
# GenerateNewS <- function(M,B){
#   for(i in 1:9){
#     source_c <- which(B[i,] == 1)
#     if(length(source_c) > 1){
#       change <- sample(source_c,2)
#       mid <- M[i,change[1]]
#       M[i,change[1]] <- M[i,change[2]]
#       M[i,change[2]] <- mid
#     }
#   }
#   return(M)
# }
# 
# #version 1.2
# GenerateNewS <- function(M,B){
#   for(i in 1:9){
#     source_c <- which(B[i,] == 1)
#     if(length(source_c) > 1){
#       M[i,source_c] <- sample(M[i,source_c], length(source_c))
#     }
#   }
#   return(M)
# }

#vesion 2
set.seed(20190423)
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


GenerateNewS <- function(M,B){
  for (i in 1:3) {
    for (j in 1:3){
      exchange <- numeric()
      for (k in (i*3-2):(i*3)){
        for (l in (j*3-2):(j*3)){
          if (B[k,l] == 1){
            exchange <- rbind(exchange, c(k,l))
          }
        }
      }
      excandidate <- numeric()
      canindex <- numeric()

      for (m in 1:nrow(exchange)){
        indivialmistake <- 0
        indivialmistake <- indivialmistake + length(which(M[exchange[m,][1],] == M[exchange[m,][1],exchange[m,][2]])) - 1 + length(which(M[,exchange[m,][2]] == M[exchange[m,][1],exchange[m,][2]])) - 1
        if (indivialmistake > 0){
          excandidate <- rbind(excandidate, exchange[m,])
          canindex <- c(canindex, m)
        }
      }
      
      if (length(excandidate) != 0){
        if (nrow(excandidate) > 2){
          nrow_e <- nrow(excandidate)
          change <- sample(nrow_e, 2)
          inter <- M[excandidate[change[1],][1], excandidate[change[1],][2]]
          M[excandidate[change[1],][1], excandidate[change[1],][2]] <- M[excandidate[change[2],][1], excandidate[change[2],][2]]
          M[excandidate[change[2],][1], excandidate[change[2],][2]] <- inter
        }else{
          if (nrow(excandidate) == 2){
            nrow_e <- nrow(excandidate)
            change <- sample(nrow_e, 2, replace = T)
            inter <- M[excandidate[change[1],][1], excandidate[change[1],][2]]
            M[excandidate[change[1],][1], excandidate[change[1],][2]] <- M[excandidate[change[2],][1], excandidate[change[2],][2]]
            M[excandidate[change[2],][1], excandidate[change[2],][2]] <- inter 
          }else{
            nrow_e <- nrow(exchange)
            change <- sample(c(1:nrow_e)[-canindex], 1)
            inter <- M[exchange[change,][1], exchange[change,][2]]
            M[exchange[change,][1], exchange[change,][2]] <- M[excandidate[1], excandidate[2]]
            M[excandidate[1], excandidate[2]] <- inter
          }
        }
      # }else{
      #   nrow_e <- nrow(exchange)
      #   change <- sample(nrow_e, 2)
      #   inter <- M[exchange[change[1],][1], exchange[change[1],][2]]
      #   M[exchange[change[1],][1], exchange[change[1],][2]] <- M[exchange[change[2],][1], exchange[change[2],][2]]
      #   M[exchange[change[2],][1], exchange[change[2],][2]] <- inter
      }

    }
  }
  return(M)
}

# version 2.1
# GenerateNewS <- function(M,B){
#   for (i in 1:3) {
#     for (j in 1:3){
#       exchange <- numeric()
#       for (k in (i*3-2):(i*3)){
#         for (l in (j*3-2):(j*3)){
#           if (B[k,l] == 1){
#             exchange <- rbind(exchange, c(k,l))
#           }
#         }
#       }
#       excandidate <- numeric()
#       canindex <- numeric()
#       gridmistake <- 0
#       for (m in 1:nrow(exchange)){
#         indivialmistake <- 0
#         indivialmistake <- indivialmistake + length(which(M[exchange[m,][1],] == M[exchange[m,][1],exchange[m,][2]])) - 1 + length(which(M[,exchange[m,][2]] == M[exchange[m,][1],exchange[m,][2]])) - 1
#         if (indivialmistake > 0){
#           excandidate <- rbind(excandidate, exchange[m,])
#           canindex <- c(canindex, m)
#           gridmistake <- gridmistake + indivialmistake
#         }
#       }
#       
#       # if (length(excandidate) != 0){
#       #   if (nrow(excandidate) > 2){
#       #     nrow_e <- nrow(excandidate)
#       #     change <- sample(nrow_e, 2)
#       #     inter <- M[excandidate[change[1],][1], excandidate[change[1],][2]]
#       #     M[excandidate[change[1],][1], excandidate[change[1],][2]] <- M[excandidate[change[2],][1], excandidate[change[2],][2]]
#       #     M[excandidate[change[2],][1], excandidate[change[2],][2]] <- inter
#       #   }else{
#       #     if (nrow(excandidate) == 2){
#       #       nrow_e <- nrow(excandidate)
#       #       change <- sample(nrow_e, 2, replace = T)
#       #       inter <- M[excandidate[change[1],][1], excandidate[change[1],][2]]
#       #       M[excandidate[change[1],][1], excandidate[change[1],][2]] <- M[excandidate[change[2],][1], excandidate[change[2],][2]]
#       #       M[excandidate[change[2],][1], excandidate[change[2],][2]] <- inter 
#       #     }else{
#       #       nrow_e <- nrow(exchange)
#       #       change <- sample(c(1:nrow_e)[-canindex], 1)
#       #       inter <- M[exchange[change,][1], exchange[change,][2]]
#       #       M[exchange[change,][1], exchange[change,][2]] <- M[excandidate[1], excandidate[2]]
#       #       M[excandidate[1], excandidate[2]] <- inter
#       #     }
#       #   }
#         # }else{
#       if(gridmistake != 0){
#         nrow_e <- nrow(exchange)
#         change <- sample(nrow_e, 2)
#         inter <- M[exchange[change[1],][1], exchange[change[1],][2]]
#         M[exchange[change[1],][1], exchange[change[1],][2]] <- M[exchange[change[2],][1], exchange[change[2],][2]]
#         M[exchange[change[2],][1], exchange[change[2],][2]] <- inter
#       }
#       
#     }
#   }
#   return(M)
# }
