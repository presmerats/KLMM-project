# Downsampling functions-------------------------------
down.sample <- function(x,h=4){
  i <- seq(1,nrow(x))
  x[i %% h == 0,]
}

down.sample.avg <- function(x,h=4){
  #i <- seq(1,nrow(x))
  ma <- x[1:(nrow(x)/h),]
  for(i in 1:nrow(x)){
    if (i %% h == 0  && i>h){
      sum <- x[i,]
      for (j in 1:h){
        sum <- sum + x[i-j,]  
      }
      sum <- sum/h
      ma[i/h,] = sum
    }
  }
  return(ma)
}
