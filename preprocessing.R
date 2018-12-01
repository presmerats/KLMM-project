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
  for (downsampling in c(33,100)){
    for (ma.window in c(100)){
      for(w in c(50,100,500)){
        for (d in c(50,100)){
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

verification <- function(){
  # load file
  load(file = "csv_small_50_20_100_100.Rdata")
  df <- df3
  
  df$group
  length(unique(df$group))
  
  nrow(df[df$group=="2",])
  nrow(df)
  ncol(df)
  df[89:91,c(2,3,49,50,51,206)]    # we observe that the "lagging" of values from x1_i to x1_j stops from row 90 to 91
  # -> ok , as the group changes the values shhould no longer be "lagged"
}

#data.preparation.prev("./data/raw/csv_small2","csv_small2")
data.preparation.prev("./data/raw/csv_small5","./data/preprocessed/csv_small5")
data.preparation.prev("./data/raw/csv_small50","./data/preprocessed/csv_small50")
data.preparation.prev("./data/raw/csv_small75","./data/preprocessed/csv_small75")
data.preparation.prev("./data/raw/csv_small100","./data/preprocessed/csv_small100")
data.preparation.prev("./data/raw/csv_small150","./data/preprocessed/csv_small150")
data.preparation.prev("./data/raw/csv_small200","./data/preprocessed/csv_small200")

#preprocessing.final("csv_small2")
prefix = "csv_small"
w=50
d=20
downsampling=100
ma.window=100
data.preparation(prefix,w,d,downsampling,ma.window)

# already executed
#data.preparation.prev("./data/raw/csv","csv_all")
# pending
#preprocessing.all()





