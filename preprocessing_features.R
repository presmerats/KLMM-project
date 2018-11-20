
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
  return(dff)
}
