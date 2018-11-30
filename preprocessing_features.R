
mylag <- function(x, k) c(rep(0, k), x[1:(length(x)-k)]) 

features.time.window <- function(df, w=2){
  # the data frame must be of the form
  # names = c('time','x1','x2','x3','error','group')
  
  i = w
  dff <-  data.frame(df$time)
  #head(dff)
  for (k in 2:(ncol(df)-2) ) {
    #x <- zoo(df[,k])
    #class(x)
    #head(x)
    x <- df[,k]
    dfnew <- data.frame(df[,k])
    head(dfnew)
    for (j in 1:i){
      lagtest <- mylag(x, j)
      class(lagtest)
      head(lagtest)
      dfnew <- cbind(dfnew,lagtest )
      head(dfnew)
      
    }
    # improve with x"k"l"j
    #thenames <- c(1:i)
    #colnames(dfnew) <- c(colnames(dfnew),thenames)

    dff <- cbind(dff,dfnew)
  }  
  dff <- cbind(dff,df[,(ncol(df)-1):ncol(df)])
  return(dff[w:nrow(dff),])
}

features.prediction <- function(df, d=2){
  # the data frame must be of the form
  # names = c('time','x1','x2','x3','error','group')
  i = d
  # construct a col vector with all y from future d positions

  futurey <- rep(0,nrow(df)-d)
  for (k in 1:(nrow(df)-d)){
    futurey[k] <- df$y[k+d]
  }
  # remove last d rows
  df2 <- df[1:(nrow(df)-d),]
  # add future column
  df2$futurey <- futurey
  return(df2)
}


data.preparation <- function( dirpath, w, d, dsw, maw){
  
  
  df <- read_files(dirpath)
  print(dim(df))
  

  # downsampling
  df.downsampled <- down.sample.avg(df,dsw)

  
  # ma on error 
  df.downsampled$y <- smoothing(df.downsampled$error, w=maw )
  
  # apply w window
  df.downsampled <- features.time.window(df.downsampled,w)  
  
  # apply d future pred -> new column future.y
  
  if (d>nrow(df.downsampled)-1) return
  df.downsampled <- features.prediction(df.downsampled,d)
  print(dim(df.downsampled))
  df.downsampled$futurey
  
  
  # save data
  filename <- paste("csv_small",w,d,dsw,maw,sep="_")
  filenamer <- paste(filename,".Rdata",sep="")
  save(df.downsampled, file = filenamer)
  
  #filenamecsv <- paste(filename,".csv",sep="")
  #write.csv(df.downsampled, file = filenamecsv)
  
  
}


