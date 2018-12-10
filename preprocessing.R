rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")

preprocessing.final <- function(prefix){
  
  library(parallel)
  numCores <- detectCores()
  numCores
  library(foreach)
  library(doParallel)
  
  start_time <- Sys.time()
  #data.preparation(dirpath,w,d,downsampling_w, ma_w)
  # for (downsampling in c(4,33,100,330,1000)){
  #   for (ma.window in c(50,100,200)){
  #     for(w in c(10,20,50,100,500,1000)){
  #       for (d in c(10,20,50,100,500,1000)){
  
  experiments <- data.frame()
  #for (downsampling in c(33,100,330)){
  for (downsampling in c(10)){
    for (ma.window in c(250)){
      for(w in c(50,100,200,500,1000)){
        for (d in c(50,100,200)){
          experiments <- rbind(experiments, data.frame(downsampling=downsampling, ma.window=ma.window, w=w, d=d))
        }}}}

  registerDoParallel(numCores-3)
  foreach (i=1:nrow(experiments)) %dopar% {
    w = experiments$w[i]
    d = experiments$d[i]
    downsampling = experiments$downsampling[i]
    ma.window = experiments$ma.window[i]
    try(data.preparation(prefix,w,d,downsampling,ma.window)  )
  }
  
  end_time <- Sys.time()
  total.time = end_time - start_time
  print(paste("total time ",prefix))
  print(total.time)
  
}

done <- function(){

#data.preparation.prev("./data/raw/csv_small2","csv_small2")
data.preparation.prev("./data/raw/csv_small5","./data/preprocessed/csv_small5_test")
data.preparation.prev("./data/raw/csv_small50","./data/preprocessed/csv_small50")
data.preparation.prev("./data/raw/csv_small75","./data/preprocessed/csv_small75")

#data.preparation.prev("./data/raw/csv","csv_all")

# Downsampling and moving average study
testing.downsampling
study.downsampling.values()
study.ma.values()

# prefix = "csv_small"
# w=50
# d=20
# downsampling=100
# ma.window=100
# data.preparation(prefix,w,d,downsampling,ma.window)
preprocessing.final("csv_small5")
preprocessing.final("csv_small50")
preprocessing.final("csv_small75")


data.preparation.prev("./data/raw/csv_small100","./data/preprocessed/csv_small100")
preprocessing.final("csv_small100")

data.preparation.prev("./data/raw/csv_small150","./data/preprocessed/csv_small150")
preprocessing.final("csv_small150")

data.preparation.prev("./data/raw/csv_small200","./data/preprocessed/csv_small200")
preprocessing.final("csv_small200")


preprocessing.final("csv_all")

#preprocessing.final("csv_small5")


data.preparation("csv_small5",w=500,d=100,10,50)

data.preparation("csv_small5",w=500,d=100,1,10)


}



