source("preprocessing_downsampling.R")



read.dataset <- function(){
  
  df_error <- read.csv(file = "./data/raw/seleccio/Error_150.csv", header = TRUE, sep=",")
  df_x1 <- read.csv(file = "./data/raw/seleccio/S1_150.csv", header = TRUE, sep=",")
  df_x2 <- read.csv(file = "./data/raw/seleccio/S2_150.csv", header = TRUE, sep=",")
  df_x3 <- read.csv(file = "./data/raw/seleccio/S3_150.csv", header = TRUE, sep=",")
  
  #add a different group for every 16000 values
  n <- nrow(df_x1)
  group <- rep(0,n)
  for (i in 1:n){
    group[i] <- floor((i-1)/16000)
  }
  return(data.frame(x1=df_x1$x,x2=df_x2$x,x3=df_x3$x,error=df_error$x, group=group))
}

plot.dataset <- function(df,nmin=1,nmax=1000){
  par(mfrow=c(2,1))
  plot(df$x1[nmin:nmax], type="l", col="yellow", ylim=c(-1,1))
  lines(df$x2[nmin:nmax], type="l", col="blue")
  lines(df$x3[nmin:nmax], type="l", col="magenta")
  plot(df$error[nmin:nmax], type="l", col="black")
  
}


add.slopes <- function(df, w,d){
  n = nrow(df)
  prev.x1 = c(rep(0,w),df$x1[1:(n-w)])
  prev.x2 = c(rep(0,w),df$x2[1:(n-w)])
  prev.x3 = c(rep(0,w),df$x3[1:(n-w)])
  prev.y  = c(rep(0,w),df$error[1:(n-w)])
  
  slope.x1 = (df$x1 - prev.x1)/w
  slope.x2 = (df$x2 - prev.x2)/w
  slope.x3 = (df$x3 - prev.x3)/w
  
  return(cbind(df,slope.x1,slope.x2,slope.x3))
}


discretization <- function(df.new, p, q, maxBer){
  
  # p is the number of ranges for sop vars (between -1 and 1)
  interval = 1 - (-1)
  interval.length = interval/p
  # discretize xi
  dx1 <- cut(df.new$x1, breaks = seq(-1.0,1.0,interval.length) ) 
  dx2 <- cut(df.new$x2, breaks = seq(-1.0,1.0,interval.length) ) 
  dx3 <- cut(df.new$x3, breaks = seq(-1.0,1.0,interval.length) ) 
  # how to discretize trend?? slope can be -inf, inf....
  # approach 1) q-2 between -1000,1000 then 2 other ranges under-1000, above1000
  max.slope=1000
  #interval = 2*max.slope
  #interval.length = interval/(q-2)
  #the.ranges = c(-.Machine$double.xmax, seq(-max.slope,max.slope,interval.length), .Machine$double.xmax)
  # logarithmic interval
  # we observe slope varies between 1e-3 and 1e-1
  q2 = q 
  q2 = q2/2
  log.seq = c(1 %o% 10^((-q2/2):(-1)))
  the.ranges = c(-.Machine$double.xmax, -1*rev(log.seq) ,  log.seq, .Machine$double.xmax)
  dsx1 <- cut(df.new$slope.x1, breaks = the.ranges)
  dsx2 <- cut(df.new$slope.x2, breaks = the.ranges)
  dsx3 <- cut(df.new$slope.x3, breaks = the.ranges)
  # approach 2) user polar coordinates approach? with x=1 y=slope
  
  # q is the number of ranges for BER error (between 0 and ? max(BER) in all dataset)
  interval = maxBer 
  interval.length = interval/q
  dy <- cut(df.new$error, breaks = seq(0, maxBer, interval.length))
  dfuturey <- cut(df.new$futurey, breaks = seq(0, maxBer, interval.length))
  
  return(data.frame(x1=dx1,x2=dx2,x3=dx3,sx1=dsx1,sx2=dsx2,sx3=dsx3,error=dy,futurey=dfuturey))
}

data.preparation.previous.work <- function(w,d, output){
  
  df = read.dataset()
  
  
  # for each group
  maxBer <- max(df$error)
  groups <- unique(df$group)
  df3 <- data.frame()
  for (i in 1:length(groups)){
    
    dfsubset <- df[df$group==groups[i],]
    
    # downsample to 1ms
    df.downsampled <- down.sample.avg(dfsubset,4)
    rm("dfsubset")
    n <- nrow(df.downsampled)
    
    # compute the slope and add as a new column for each SOP var
    df.new <- add.slopes(df.downsampled,w,d)
    rm("df.downsampled")
    
    # add the future value of ber
    futurey = c(df.new$error[d:(n-1)],rep(0,d))
    df.new <- cbind(df.new, futurey)
    
    # discretize -> convert to p=16 and q=8 ranges
    df.new <- discretization(df.new, p=16, q=8,maxBer)
    
    # group 
    df.new$group <- rep(groups[i],nrow(df.new))
    
    # bind with result dataset
    df3 <- rbind(df3,df.new[(w+1):(n-d),])
    rm("df.new")
    
  }
  
  # save to disk
  filename <- paste("./data/preprocessed/",output,".Rdata",sep="")
  save(df3, file = filename)
}
