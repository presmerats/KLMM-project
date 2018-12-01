rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")


#testing the limitations in data set size -------------------------------------------------
w=100;d=100;dsw=50;maw=100
#data.preparation.prev("./data/raw/csv_small5","csv_small5")
res = data.preparation("csv_small5",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11) # 1 sec training
#data.preparation.prev("./data/raw/csv_small50","csv_small50")
res = data.preparation("csv_small50",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11) # 20 min training

#  playing with different downsampling values --------------------------------
w=110;d=110;dsw=50;maw=100  # 1 each 50*0.25ms , d=110 => 110*50*0.25ms=1.52s predict window
res = data.preparation("csv_small5",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11) 
w=120;d=110;dsw=40;maw=100 # 1 each 40*0.25ms , d=110 => 110*40*0.25ms=1.2s predict window
res = data.preparation("csv_small5",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11) 
w=130;d=110;dsw=30;maw=100 # 1 each 30*0.25ms , d=110 => 110*30*0.25ms=0.91s predict window
res = data.preparation("csv_small5",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small5", w,d,dsw,maw,sigma=1e-11) 


# too long for now?-----------------------------------------------------------
#data.preparation.prev("./data/raw/csv_small75","csv_small75")
res = data.preparation("csv_small75",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small75", w,d,dsw,maw,sigma=1e-11)
#data.preparation.prev("./data/raw/csv_small100","csv_small100")
res = data.preparation("csv_small100",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("./data/preprocessed/csv_small100",  w,d,dsw,maw,sigma=1e-11)
#data.preparation.prev("./data/raw/csv_small150","csv_small150")
res = data.preparation("csv_small150",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("./data/preprocessed/csv_small150",  w,d,dsw,maw,sigma=1e-11)
#data.preparation.prev("./data/raw/csv_small200","csv_small200")
res = data.preparation("csv_small200",w,d,dsw,maw)
if (!is.null(res)) train.test.rbf("csv_small200", w,d,dsw,maw,sigma=1e-11)



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




