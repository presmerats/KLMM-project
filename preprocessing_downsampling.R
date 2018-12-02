# Downsampling functions-------------------------------
down.sample <- function(x,h=4){
  i <- seq(1,nrow(x))
  x[i %% h == 0,]
}

down.sample.avg <- function(x,h=4){
  #i <- seq(1,nrow(x))
  ma <- x[1:(nrow(x)/h),]
  #print(paste("h: ",h))
  for(i in h:nrow(x)){
    #print(paste("i: ",i, " i %% h == ",i %% h))
    if (i %% h == 0  && i>0){
      
      #print(paste("i: ",i," h:",h))
      sum <- x[i,]
      for (j in 0:(h-1) ){
        #print(paste("setting  ",i-j))
        sum <- sum + x[i-j,]  
      }
      sum <- sum/h
      ma[i/h,] = sum
    }
  }
  return(ma)
}


down.sample.avg.retain.num.points <- function(x,h=4){
  #i <- seq(1,nrow(x))
  ma <- x
  
  for(i in h:nrow(x)){
    #print(paste("i: ",i, " i %% h == ",i %% h))
    if (i %% h == 0  && i>0){
      sum <- NULL
      #print(paste("i: ",i," h:",h))
      #print(sum)
      for (j in 0:(h-1) ){
        #print(paste("setting  ",i-j))
        
        if (is.null(sum)) sum <- x[i-j,]
        else sum <- sum + x[i-j,]  
        #print(paste("Addindg"))
        #print(x[i-j,])
        #print("update sum=")
        #print(sum)
      }
      sum <- sum/h
      #print("result sum=")
      #print(sum)
      # all values from i-h to i must be now sum
      for (k in (i-h+1):i){ma[k,] <- sum}
      #print("----------------")
    }
  }
  return(ma)
}
# 
# down.sample.avg.retain.num.points <- function(x,h=4){
#   #i <- seq(1,nrow(x))
#   print(nrow(x))
#   ma <- x
#   sum <- x[1,]
#   for(i in h:nrow(x)){
#     if ((i-1) %% h == 0  && i>h){
#       sum <- x[i,]
#       for (j in 1:h){
#         sum <- sum + x[i-j,]  
#       }
#       sum <- sum/h
#       # all values from i-h to i must be now sum
#       for (k in (i-h+1):i){ma[k,] <- sum}
#       
#     }
#     
#   }
#   return(ma)
# }

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



testing.downsampling <- function(){
  
  # verification that the downsampling works
  y <- c(1,2,1,2,1,3,2,3,2,3,4,3,4,3,5,4,7,5,9,8,9,8,9,8,7,8,7,8,6,8,5,8,4,5,4,5,4,5,7,5,6,7,5,3,6,4,2,5,6,1,2,1,2,3,1,4,2,0,3,2,1)
  df <- data.frame(x=y,y)
  # each point 0.25ms
  dsw=5 # ms
  dswp=dsw/0.25 # 20
  maw=10 # ms
  map=maw/dsw
  df.downsampled <- down.sample.avg.retain.num.points(df,dswp)
  
  df.downsampled <- down.sample.avg(df,dswp)
  df.downsampled
  nrow(df.downsampled)
  # now upsample to dswp per each value
  dfres <- data.frame(x=y,y)
  dfres
  for (i in 1:nrow(df.downsampled)){
    #print("----------------")
    #print(i)
    #print("--")
    for (j in 0:(dswp-1)){
      #print(j+(i-1)*dswp)
      dfres[(i-1)*dswp+j,] <- df.downsampled[i,]
    }
  }
  dfres
  df.downsampled <- dfres
  
  df.downsampled$x <- y[1:length(df.downsampled$x)]
  length(df.downsampled$x)
  length(y)
  df.downsampled$y.2 <- y
  df.downsampled$y.2 <- smoothing(df.downsampled$error,map )
  df.downsampled
  plot(df.downsampled$x, type="l", col="black")
  lines(df.downsampled$y, type="l", col="blue")
  lines(df.downsampled$y.2, type="l", col="red")
  
}




study.downsampling.value <- function(w,d,dsw,maw){
  # w in ms
  # d in ms
  # dsw in data points
  # maw in data points 
  # 0.25 ms/ original data point
  
  dswp = dsw/0.25
  map = maw/0.25/dswp
  wp=round(w/0.25/dswp,0)
  dp=round(d/0.25/dswp,0)
  
  #print(dswp)
  
  label = paste("Time in ms (w=",w,"ms d=",d,"ms dsw=",dsw,"ms ma=",maw,"ms "," wp=",wp," dp=",dp," dswp=",dswp," map=",map ,")" ,sep="")
  
  filename = "./data/preprocessed/csv_small5_raw.Rdata"
  load(file = filename) # loads a df named df
  df2 <- df[1:10000,]
  #print("smootihg to ")
  #print(map)
  df.downsampled <- down.sample.avg.retain.num.points(df2,dswp)
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame ----------------------")
  # 
  df.downsampled$y <- smoothing(df.downsampled$error,map )
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame again-----------------")
  #print(df.downsampled$error[1100:1200])
  #print(df.downsampled$y[1100:1200])
  
  #print(nrow(df))
  #print(nrow(df.downsampled))
  #plot(df$error)
  plotname = gsub(":","_",label)
  plotname = gsub(" ","_",plotname)
  plotname = gsub("=","_",plotname)
  plotname = gsub(",","-",plotname)
  
  # x in ms
  x.ms = c(1:nrow(df.downsampled))
  # print("length problem")
  # print(length(df.downsampled$error))
  # print(length(df2$error))
  # print(length(x.ms))
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame ----------------------")
  
  #print(x.ms[c(1:10,1000:1010)])
  maxy = max(max(df2$error),max(df.downsampled$error),max(df.downsampled$y))
  miny = min(min(df2$error),min(df.downsampled$error),min(df.downsampled$y))
  #print(c(maxy,miny))
  xmin=2000
  xmax=6000
  x.ms <- x.ms[xmin:xmax]
  #print(x.ms[c(1,2,length(x.ms)-1,length(x.ms))])
  plot1 <- df2$error[c(xmin:xmax)]
  plot2 <- df.downsampled$error[xmin:xmax]
  plot3 <- df.downsampled$y[xmin:xmax]
  
  
  jpeg(paste("./plots/downsampling_",plotname,".jpeg",sep="",collapse=""))
  plot(x.ms,plot1,xlab=label,main=paste("Downsampling to",dsw," ms",sep=""),ylim=c(miny,maxy),ylab="BER", type="l", pch=2, lty=3)
  lines(x.ms,plot2,col="blue", type="l",pch=19, lty=1,ylim=c(miny,maxy), lwd="3" )
  #lines(x.ms,plot3,col="red",  type="l",pch=20, lty=3,ylim=c(miny,maxy))
  legend( xmin,maxy, c("orig","downsampled"), cex=0.8, col=c("black","blue"), pch=c(2,19), lty=c(3,1));
  
  dev.off()
  
  
  plot(x.ms,plot1,xlab=label,main=paste("Downsampling 1 value/",dsw,"ms",sep=""),ylim=c(miny,maxy),ylab="BER", type="l", pch=2, lty=3)
  lines(x.ms,plot2,col="blue", type="l",pch=19, lty=1,ylim=c(miny,maxy), lwd="3" )
  #lines(x.ms,plot3,col="red",  type="l",pch=20, lty=3,ylim=c(miny,maxy))
  #plot(plot1)
  legend( xmin,maxy, c("orig","downsampled"), cex=0.8, col=c("black","blue"), pch=c(2,19), lty=c(3,1));
  
}

study.downsampling.values <- function(){
  # for (w in c(50,100,200,250,500)){  
  #   for (d in c(25,50,100,200)){  
  #     for (dsw in c(1,2,4,8,10)){  
  #       for (maw in c(10,50,100)){  
  #         print(study.ma.value(w,d,dsw,maw))
  #       }}}}
  for (w in c(100)){  
    for (d in c(50)){ 
      # 1, 4, 10
      for (dsw in c(10,4,1)){ # downsample to 1 value each 10ms   
        #for (maw in c(20,50,100)){  
        for (maw in c(100)){  # moving average of last values in 100ms
          print(study.downsamling.value(w,d,dsw,maw))
        }}}}
}



study.ma.value <- function(w,d,dsw,maw){
  # w in ms
  # d in ms
  # dsw in data points
  # maw in data points 
  # 0.25 ms/ original data point
  
  dswp = dsw/0.25
  map = maw/0.25/dswp
  wp=round(w/0.25/dswp,0)
  dp=round(d/0.25/dswp,0)
  
  #print(dswp)
  
  label = paste("Time in ms (w=",w,"ms d=",d,"ms dsw=",dsw,"ms ma=",maw,"ms "," wp=",wp," dp=",dp," dswp=",dswp," map=",map ,")" ,sep="")
  
  filename = "./data/preprocessed/csv_small5_raw.Rdata"
  load(file = filename) # loads a df named df
  df2 <- df[1:10000,]
  #print("smootihg to ")
  #print(map)
  df.downsampled <- down.sample.avg.retain.num.points(df2,dswp)
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame ----------------------")
  # 
  df.downsampled$y <- smoothing(df.downsampled$error,map )
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame again-----------------")
  #print(df.downsampled$error[1100:1200])
  #print(df.downsampled$y[1100:1200])
  
  #print(nrow(df))
  #print(nrow(df.downsampled))
  #plot(df$error)
  plotname = gsub(":","_",label)
  plotname = gsub(" ","_",plotname)
  plotname = gsub("=","_",plotname)
  plotname = gsub(",","-",plotname)
  
  # x in ms
  x.ms = c(1:nrow(df.downsampled))
  # print("length problem")
  # print(length(df.downsampled$error))
  # print(length(df2$error))
  # print(length(x.ms))
  # print(nrow(df2))
  # print(nrow(df.downsampled))
  # print("maked sure this is the ssame ----------------------")
  
  #print(x.ms[c(1:10,1000:1010)])
  maxy = max(max(df2$error),max(df.downsampled$error),max(df.downsampled$y))
  miny = min(min(df2$error),min(df.downsampled$error),min(df.downsampled$y))
  #print(c(maxy,miny))
  xmin=2000
  xmax=6000
  x.ms <- x.ms[xmin:xmax]
  #print(x.ms[c(1,2,length(x.ms)-1,length(x.ms))])
  plot1 <- df2$error[c(xmin:xmax)]
  plot2 <- df.downsampled$error[xmin:xmax]
  plot3 <- df.downsampled$y[xmin:xmax]
  
  
  jpeg(paste("./plots/ma_",plotname,".jpeg",sep="",collapse=""))
  plot(x.ms,plot1,xlab=label,main=paste("Moving average  in a ",maw," ms window",sep=""),ylim=c(miny,maxy),ylab="BER", type="l", pch=2, lty=3)
  #lines(x.ms,plot2,col="red", type="l",pch=19, lty=1,ylim=c(miny,maxy), lwd="3" )
  lines(x.ms,plot3,col="red",  type="l",pch=19, lty=1,ylim=c(miny,maxy),lwd="3" )
  legend( xmin,maxy, c("orig","downsampled with moving average"), cex=0.8, col=c("black","red"), pch=c(2,19), lty=c(3,1));
  
  dev.off()
  
  
  plot(x.ms,plot1,xlab=label,main=paste("Moving average  in a ",maw," ms window",sep=""),ylim=c(miny,maxy),ylab="BER", type="l", pch=2, lty=3)
  #lines(x.ms,plot2,col="red", type="l",pch=19, lty=1,ylim=c(miny,maxy), lwd="3" )
  lines(x.ms,plot3,col="red",  type="l",pch=19, lty=1,ylim=c(miny,maxy),lwd="3" )
  legend( xmin,maxy, c("orig","downsampled with moving average"), cex=0.8, col=c("black","red"), pch=c(2,19), lty=c(3,1));
  
}

study.ma.values <- function(){
  # for (w in c(50,100,200,250,500)){  
  #   for (d in c(25,50,100,200)){  
  #     for (dsw in c(1,2,4,8,10)){  
  #       for (maw in c(10,50,100)){  
  #         print(study.ma.value(w,d,dsw,maw))
  #       }}}}
  for (w in c(100)){  
    for (d in c(50)){ 
      # 1, 4, 10
      for (dsw in c(10)){ # downsample to 1 value each 10ms   
        for (maw in c(20,50,100,150,200,250)){  
          print(study.ma.value(w,d,dsw,maw))
        }}}}
}

