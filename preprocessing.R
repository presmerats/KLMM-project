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
  df2 <- df2[1:10000,]
  rvm(as.matrix(df2[,2:(ncol(df2)-2)]),y=df2[,ncol(df2)-1])

}


#usage()
