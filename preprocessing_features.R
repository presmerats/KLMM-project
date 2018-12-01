
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
  return(dff[(w+1):nrow(dff),])
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




data.preparation.prev <- function(dirpath, filenameprefix){
  df <- read_files(dirpath)
  filename <- paste(filenameprefix,"raw",sep="_")
  filenamer <- paste("./data/preprocessed/",filename,".Rdata",sep="")
  save(df, file = filenamer)
}

data.preparation <- function( filenameprefix, w, d, dsw, maw){
  # w and d are in ms
  #  we translate them to w' and d' that are in "data points" relative to dsw
  #  each experiments takes 4 secs and has 16000 datapoints -> 1 data point = 0.25 ms
  #  1 downsampled datapoints takes = 0.25*dsw ms
  #  w=100ms -> wp*dsw*0.25 = 100ms -> wp =100/0.25/dsw
  
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  
  
  filename = paste(filenameprefix,"raw",sep="_")
  filename = paste("./data/preprocessed/",filename,".Rdata",sep="")
  load(file = filename) # loads a df named df
  
  
  # all those steps separated by group
  groups <- unique(df$group)
  #print(groups)
  df3 <- data.frame()
  for (i in 1:length(groups)){
    
    dfsubset <- df[df$group==groups[i],]
    
    # downsampling
    df.downsampled <- down.sample.avg(dfsubset,dsw)
    # ma on error 
    df.downsampled$y <- smoothing(df.downsampled$error, w=maw )
    # apply w window
    df.downsampled <- features.time.window(df.downsampled,w)  
    # apply d future pred -> new column future.y
    if (d>nrow(df.downsampled)-1) {print(paste("Error cannot prepare dataset with d=",d," n=",(nrow(df.downsampled)-1),sep="",collapse=""));return(NULL)}
    df.downsampled <- features.prediction(df.downsampled,d)
    #print(dim(df.downsampled))
    df.downsampled$futurey
    
    # group 
    df.downsampled$group <- rep(groups[i],nrow(df.downsampled))
    #print(groups[i])
    
    # colnames
    colprefixes=c("x1","x2","x3","y")
    ini.pos.var = 0
    currentvar = 0
    oldcolnames=colnames(df.downsampled)
    newcolnames=colnames(df.downsampled)
    for (j in 1:length(oldcolnames)){
      if(oldcolnames[j]=="df...k.") {
        currentvar = currentvar + 1
        ini.pos.var = j
        newcolnames[j] = paste(colprefixes[currentvar],(j-ini.pos.var),sep="_")
      }
      else if (oldcolnames[j]=="lagtest"){
        newcolnames[j] = paste(colprefixes[currentvar],(j-ini.pos.var),sep="_")
      }
    }
    colnames(df.downsampled) <- newcolnames
    
    df3 <- rbind(df3,df.downsampled)
    rm("df.downsampled","dfsubset")
    
    
  }
  
 
  
  # save data
  filename <- paste(filenameprefix,paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp,dsw,maw,sep="_")
  filenamer <- paste("./data/preprocessed/",filename,".Rdata",sep="")
  save(df3, file = filenamer)
  
  #filenamecsv <- paste(filename,".csv",sep="")
  #write.csv(df.downsampled, file = filenamecsv)
  
  return(TRUE)
}


