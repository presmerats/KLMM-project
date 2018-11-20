rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")

# Dataframe construction---------------------------------------------------
# Reading all .csv files in a given path and returning the merge dataframe
# parameters: path (str)
read_files <- function(path) {
  files <- list.files(path = path)
  total_size <- length(files) - 2
  df <- data.frame()
  group_number <- 1
  for(f in files) {
    #skipping sampleMaxError.csv and sampleMinError.csv
    if(nchar(f) < 10) {
      file_path <- paste(path, f, sep = '/')
      print(paste("Reading:", file_path, total_size - group_number, "files left", sep = ' '))
      
      df_temp <- read.csv(file = file_path, header = TRUE, sep=",")
      df_temp['group'] <- group_number
      
      df <- rbind(df, df_temp)
      group_number <- group_number + 1
    }
  }
  
  keeps <- c("V1","V2","V3","V4","V6",'group')
  df_final <- df[keeps]
  names(df_final) <- c('time','x1','x2','x3','error','group')
  
  print("Saving merge dataframe into data.Rdata file...")
  save(df_final, file = 'data.Rdata')
  return(df_final)
}



# Usage example ----------------------------------------------------

usage <- function(){
  # save the complete dataset
  #read_files("./data/raw/csv")
  # load a small dataset to play
  df <- read_files("./data/raw/csv_small")
  #load(file = 'data.Rdata')
  head(df)

  nrow(df)
  max(df$x3)
  summary(df)
  plot.basic(df, title="Original data 0.278ms/sample ")
  
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


#usage()
