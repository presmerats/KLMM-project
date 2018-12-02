rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")


# looking for a valid rbf kernel--------------------------------------------
fileprefix="csv_small2"
train.test.auto(fileprefix, w=100,d=50,dsw=100,maw=100)
train.test.vanilladot(fileprefix, w=100,d=50,dsw=100,maw=100) # error
train.test.rbf(fileprefix, w=100,d=50,dsw=100,maw=100,sigma=0.1)
train.test.rbf(fileprefix, w=100,d=50,dsw=100,maw=100,sigma=1e-7)
train.test.rbf(fileprefix, w=100,d=50,dsw=100,maw=100,sigma=1e-12) # good
train.test.rbf(fileprefix, w=100,d=50,dsw=100,maw=100,sigma=1e-14) # good
train.test.rbf(fileprefix, w=50,d=100,dsw=100,maw=100,sigma=1e-14) # fails
train.test.rbf(fileprefix, w=50,d=100,dsw=100,maw=100,sigma=1e-11) # good
train.test.rbf(fileprefix, w=50,d=50,dsw=100,maw=100,sigma=1e-11) # good


# fixed downsampling and moving average
dsw=10;maw=250

#testing the limitations in data set size -------------------------------------------------
w=100;d=100;
train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11)  # error!
preprocessing.final("csv_small5")
preprocessing.final("csv_small50")
preprocessing.final("csv_small75")
w=500;d=100;
train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11)  # 15min
w=1000;d=100;
train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11)  # 2min training
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11) # 
train.test.rbf("csv_small75", w,d,dsw,maw,sigma=1e-11)

data.preparation.prev("./data/raw/csv_small100","./data/preprocessed/csv_small100")
preprocessing.final("csv_small100")
train.test.rbf("csv_small100", w,d,dsw,maw,sigma=1e-11)

data.preparation.prev("./data/raw/csv_small150","./data/preprocessed/csv_small150")
preprocessing.final("csv_small150")
train.test.rbf("csv_small150", w,d,dsw,maw,sigma=1e-11)

data.preparation.prev("./data/raw/csv_small200","./data/preprocessed/csv_small200")
preprocessing.final("csv_small200")
train.test.rbf("csv_small200", w,d,dsw,maw,sigma=1e-11)




