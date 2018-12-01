
# Study of target values ------------------------------------

# smoothing
smoothing.study <- function(){
  # save the complete dataset
  #read_files("./data/raw/csv")
  # load a small dataset to play
  #df <- read_files("./data/raw/csv_small")
  #df <- read_files("./data/new1")
  load(file = 'data.Rdata')
  df <- df_final
  head(df)
  
  nrow(df)
  max(df$x3)
  summary(df)
  plot.basic(df, title="Original data 0.278ms/sample ")
  
  # smoothing error tests first 10000 data points
  n <- nrow(df)
  #n <- 60000
  df2 <- df[1:n,]
  
  
  par(mfrow=c(1,1))
  plot(df2$error, main="smoothing window=10 points ")
  
  w<-10
  coefs <- exponential.decay.coefs(w,i=0.7)
  df2$y2 <- smoothing(df2$error, w, coefs)
  points(df2$y2, col="blue")
  
  df2$y <- smoothing(df2$error, w)
  points(df2$y, col="red")
  legend(100,90000, legend=c("original","MA", "Weighted MA"),
         col=c("black","red", "blue"), pch=c(1,1,1), cex=0.8)  
  
  plot.basic(df2, title="Original data 0.278ms/sample ")
  df2$error <- df2$y
  plot.basic(df2, title=paste("Smoothed error w=",w,sep=""))
  
  # verifications, mean the same, median the same, std lower, max, min smaller
  summary(df)
  summary(df2)  
  
  
  # w=100
  df2 <- df[1:n,]
  par(mfrow=c(1,1))
  plot(df2$error, main="smoothing window=100 points ")
  
  # weighted MA
  w<-100
  coefs <- exponential.decay.coefs(w,i=0.7)
  df2$y2 <- smoothing(df2$error, w, coefs)
  points(df2$y2, col="blue")
  
  # MA
  df2$y <- smoothing(df2$error, w)
  points(df2$y, col="red")
  legend(100,90000, legend=c("original","MA", "Weighted MA"),
         col=c("black","red", "blue"), pch=c(1,1,1), cex=0.8)  
  # comparision
  plot.basic(df2, title="Original data  ")
  df2$error <- df2$y
  plot.basic(df2, title=paste("Smoothed error w=",w,sep=""))
  
  # verifications,
  summary(df)
  summary(df2)  
  
}
#smoothing.study()


# new data target noise comparison
comparison.new.data <- function(){
  #load(file = 'data.Rdata')
  #df <- df_final
  df.orig <- read_files("./data/raw/csv_small")
  summary(df.orig)
  
  df1 <- read_files2("./data/new1",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df1)
  df2 <- read_files2("./data/new2",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df2)
  df3 <- read_files2("./data/new3",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df3)
  df4 <- read_files2("./data/new4",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df4)
  df5 <- read_files2("./data/new5",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df5)
  sd(df.orig$error)
  sd(df1$error)
  sd(df2$error)
  sd(df3$error)
  sd(df4$error)
  sd(df5$error)
  # > sd(df.orig$error)
  # [1] 2465.224
  # > sd(df1$error)
  # [1] 2120.768
  # > sd(df2$error)
  # [1] 1394.844
  # > sd(df3$error)
  # [1] 484.4986
  # > sd(df4$error)
  # [1] 166.4815
  # > sd(df5$error)
  # [1] 71.26421
  
  stds <- c(  sd(df.orig$error),
              sd(df1$error),
              sd(df2$error),
              sd(df3$error),
              sd(df4$error),
              sd(df5$error))
  stds <- round(stds,2)
  
  par(mfrow=c(1,1))
  plot(df.orig$error[1:20000], ylim=c(100,90000), main="Error values and std", xlab="t", ylab="error")
  points(df1$error, col="blue")
  points(df2$error, col="red")
  points(df3$error, col="green")
  points(df4$error, col="orange")
  points(df5$error, col="cyan")
  legend(13000,50000, legend=paste(c("orig",rep("new",5)),c(0:5),rep(" std: ",6),stds),
         col=c("black","blue","red","green","orange","cyan"), pch=c(1,1,1,1,1,1), cex=0.8)  
  
  
  
}


comparison.new.data.normalized <- function(){
  #load(file = 'data.Rdata')
  #df <- df_final
  df.orig <- read_files("./data/raw/csv_small")
  summary(df.orig)
  
  df1 <- read_files2("./data/new1",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df1)
  df2 <- read_files2("./data/new2",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df2)
  df3 <- read_files2("./data/new3",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df3)
  df4 <- read_files2("./data/new4",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df4)
  df5 <- read_files2("./data/new5",theskip=21, keeps=c("X_Value", "s1",	"s2",	"s3", "n_err","group"))
  summary(df5)
  
  # scale everything
  df.orig$error <- scale(df.orig$error)
  summary(df.orig$error)
  df1$error <- scale(df1$error)
  summary(df1$error)
  df2$error <- scale(df2$error)
  summary(df2$error)
  df3$error <- scale(df3$error)
  summary(df3$error)
  df4$error <- scale(df4$error)
  summary(df4$error)
  df5$error <- scale(df5$error)
  summary(df5$error)
  
  
  
  stds <- c(  sd(df.orig$error),
              sd(df1$error),
              sd(df2$error),
              sd(df3$error),
              sd(df4$error),
              sd(df5$error))
  stds <- round(stds,2)
  
  par(mfrow=c(1,1))
  plot(df.orig$error[1:20000], ylim=c(-2,2), main="Error values and std", xlab="t", ylab="error")
  points(df1$error, col="blue")
  points(df2$error, col="red")
  points(df3$error, col="green")
  points(df4$error, col="orange")
  points(df5$error, col="cyan")
  legend(13000,0, legend=paste(c("orig",rep("new",5)),c(0:5),rep(" std: ",6),stds),
         col=c("black","blue","red","green","orange","cyan"), pch=c(1,1,1,1,1,1), cex=0.8)  
  
  
  
}

# Usage example ----------------------------------------------------

usage <- function(){
  # save the complete dataset
  #read_files("./data/raw/csv")
  # load a small dataset to play
  #df <- read_files("./data/raw/csv_small")
  #df <- read_files("./data/new1")
  load(file = 'data.Rdata')
  df <- df_final
  head(df)
  
  nrow(df)
  max(df$x3)
  summary(df)
  plot.basic(df, title="Original data 0.278ms/sample ")
  
  # smoothing error tests
  df2 <- df[1:10000,]
  
  par(mfrow=c(3,3))
  df2$y <- smoothing(df2$error, w=10, coefs=c() )
  plot(df2$error, main="smoothing window=10 points ")
  points(df2$y, col="red")
  
  # thougful downsample
  # 3600 samples per second
  # 0.000278 seconds per sample
  # 278 microseconds per sample
  # -> to ms  h = 4 almost
  dsdf <- down.sample.avg(df,4)
  nrow(dsdf)
  plot.basic(dsdf, title="Avg Downsampled to ~ 1ms")
  # -> to 10ms h=40?
  dsdf <- down.sample.avg(df,40)
  nrow(dsdf)
  plot.basic(dsdf, title="Avg Downsampled to ~ 10ms")
  # -> to 100ms h=400?
  dsdf <- down.sample.avg(df,400)
  nrow(dsdf)
  plot.basic(dsdf, title="Avg Downsampled to ~ 100ms")
  
  dsdf <- down.sample(df,4)
  nrow(dsdf)
  plot.basic(dsdf, title="Downsampled to ~ 1ms")
  # -> to 10ms h=40?
  dsdf <- down.sample(df,40)
  nrow(dsdf)
  plot.basic(dsdf, title="Downsampled to ~ 10ms")
  # -> to 100ms h=400?
  dsdf <- down.sample(df,400)
  nrow(dsdf)
  plot.basic(dsdf, title="Downsampled to ~ 100ms")
  
  # lagged series
  df2 <- features.time.window(dsdf,100)
  head(df2)
  plot.lagged(df2, limx=200)
  head(df2)
  
  # RVM
  library(kernlab)
  df2 <- features.time.window(dsdf,4)
  nrow(df2)
  ncol(df2)
  x <- as.matrix(df2[,2:(ncol(df2)-2)])
  ytrain <- df2[,ncol(df2)-1]
  head(x)
  length(x)
  nrow(x)
  length(ytrain)
  
  model <- rvm(x),y=ytrain)
# print relevance vectors
alpha(model)
RVindex(model)

#predict and plot
dftest <- read_files("./data/raw/csv_small2")
dsdf2 <- down.sample(dftest,40)
nrow(dsdf2)

xtest <- as.matrix(dsdf2[,2:(ncol(dsdf2)-2)])
nrow(xtest)
head(xtest[,2])
summary(xtest)

ytest.gt <- dsdf2[,ncol(dsdf2)-1]
summary(ytest.gt)

ytest <- predict(model, xtest)
summary(ytest)

# plot predict vs ground-truth
par(mfrow=c(1,1))
plot(ytest,type="l", main="RVM: predicted error vs real error with 4 csv files")
lines(ytest.gt, col="red")

# sum of squares error?
rmse <- sqrt(sum((ytest - ytest.gt)^2)/nrow(xtest))
rmse    
}


# usage()
