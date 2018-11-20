
# plot functions --------------------
plot.basic <- function(x,title=""){
  par(mfcol=c(2,1))
  plot(x$x1, col="green", type="l",main=title)
  points(x$x2, col="red", type="l",main=title)
  points(x$x3, col="blue", type="l",main=title)
  plot(x$error, col="black", type="l" ,main=title)
}

plot.lagged <- function(x,title="",limx=1000){
  par(mfcol=c(2,1))
  
  p=sample(rainbow(ncol(x)))
  plot(x[,2], col="green", type="l",main=title, ylim=c(-0.7,0.9), xlim=c(0,limx))
  for (k in 3:ncol(x)){
    points(x[,k], col=p[k], type="l")
  }
  
  plot(x[,ncol(x)-1], type="l", main=title)
  
}

