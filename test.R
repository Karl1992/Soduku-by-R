count <- 0
for(i in 1:9){
  row_r <- table(Solution[i,])
  count <- count + sum(row_r - 1)
}
for(j in 1:9){
  col_r <- table(Solution[,j])
  count <- count + sum(col_r - 1)
}