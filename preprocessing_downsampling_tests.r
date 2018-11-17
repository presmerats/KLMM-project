setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


plot.basic <- function(x,title=""){
  par(mfcol=c(2,1))
  plot(x$v1, col="green", type="l",main=title)
  points(x$v2, col="red", type="l",main=title)
  points(x$v3, col="blue", type="l",main=title)
  plot(x$error, col="black", type="l" ,main=title)
}

MyData <- read.csv(file="./data/raw/csv/1.csv", header=TRUE, sep=",")
head(MyData)
tail(MyData)
plot(MyData)
mean(MyData$V6)
summary(MyData)
newdf <- data.frame(v1=MyData$V2, v2=MyData$V3, v3=MyData$V4, error=MyData$V6)
head(newdf)

plot.basic(newdf)

MyData <- read.csv(file="./data/raw/csv/2.csv", header=TRUE, sep=",")
newdf <- data.frame(v1=MyData$V2, v2=MyData$V3, v3=MyData$V4, error=MyData$V6)
plot.basic(newdf)

MyData <- read.csv(file="./data/raw/csv/16.csv", header=TRUE, sep=",")
newdf <- data.frame(v1=MyData$V2, v2=MyData$V3, v3=MyData$V4, error=MyData$V6)
plot.basic(newdf)



# downsampling example
down.sample <- function(x,h=4){
  i <- seq(1,nrow(x))
  x[i %% h == 0,]
}
nrow(newdf)
plot.basic(newdf, title="Original data 0.278 ms ")

# basic downsampling---------------------------------------------------
# 3600 samples per second
# 0.000278 seconds per sample
# 278 microseconds per sample
# -> to ms  h = 4 almost
dsdf <- down.sample(newdf,4)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 1ms")
# -> to 10ms h=40?
dsdf <- down.sample(newdf,40)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 10ms")
# -> to 100ms h=400?
dsdf <- down.sample(newdf,400)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 10ms")

# downsampling with avg---------------------------------------------------
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
nrow(newdf)
plot.basic(newdf, title="Original data 0.278 ms ")

# thougful downsample
# 3600 samples per second
# 0.000278 seconds per sample
# 278 microseconds per sample
# -> to ms  h = 4 almost
dsdf <- down.sample.avg(newdf,4)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 1ms")
# -> to 10ms h=40?
dsdf <- down.sample.avg(newdf,40)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 10ms")
# -> to 100ms h=400?
dsdf <- down.sample(newdf,400)
nrow(dsdf)
plot.basic(dsdf, title="Downsampled to ~ 10ms")
