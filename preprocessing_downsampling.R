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

# smoothing/denoising target y ------------------------

smoothing <- function(y, w=10, coefs=rep(1,w)){
  # the behaviour is similar to downsampling, 
  # except that final avg at t is only set to y(t)
  # the granularity of the data doesn't change
  #
  # coefs allows for a weighted average,
  # for example introducing exponential decay...
  # coefs must be like that coefs = c( wt, wt-1, wt-2, wt-3)
  
  yres <- y
  for (t in w:length(y)){
    # compute the average from t-w to t
    # print(y[(t-w+1):t])
    # print(rev(coefs))
    # print(y[(t-w+1):t]*rev(coefs))
    # print(sum(y[(t-w+1):t]*rev(coefs)))
    # print(sum(y[(t-w+1):t]*rev(coefs))/sum(coefs))
    # print(sum(y[(t-w+1):t]/w))
    # print("")
    avg <- sum(y[(t-w+1):t]*rev(coefs))/sum(coefs)
    yres[t] <- avg
  }
  return(yres)
}


exponential.decay.coefs <- function(w, i=0.9){
  coefs = rep(1,w)
  for (j in 2:w){
    coefs[j] = coefs[j]*(i^(j-1))
  }
  return(coefs)
}
