rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("preprocessing_previous_work")

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
  data.preparation.prev("./data/raw/csv_small5","./data/preprocessed/csv_small5")
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


verification <- function(){
  
  
  df = read.dataset()
  plot.dataset(df,1,34000)
  # every 16000 values -> there's a new experiment
  # we can derive the group by counting 16000 ...
  
  df$x1[16000:16005]
  df$x2[16000:16005]
  df$x3[16000:16005]
  
  df$x1[32000:32005]
  df$x2[32000:32005]
  df$x3[32000:32005]
  
  
  
  nrow(df3)  
  
  head(df3)
  df3[1:11,c(2,5,7,10)]
  
  (df3[11,2] - df3[1,2])/ws
  
  df3[19990:20000,]
  df3[19890:19900,]
  df3[12000,]
  summary(df3$sx1)
  rm("df3")
  
  filename = paste("original_features_w10_d10","",sep="")
  filename = paste("./data/preprocessed/",filename,".Rdata",sep="")
  load(file = filename) # loads a df named df
  summary(df3)
  
  }

previous.work <- function(){
  
  # read data
  #data.preparation.prev("./data/raw/csv_small200","./data/preprocessed/csv_small200")
  
  # use csv_small from now on
  
  # preprocess: add sop trends for different values of w (write to different files)
  ws = c(10,20,30,40,50,100,150,200)
  ds = c(10,50,100,200,300)
  for (w in ws){
    for (d in ds){
      data.preparation.previous.work(
        w,d,
        output=paste('original_features',paste("w",w,sep=""),paste("d",d,sep=""),sep="_"))   
    }
    
  }
  
}
