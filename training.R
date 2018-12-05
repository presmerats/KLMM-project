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
# new tests
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=1500) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=3000) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=4000) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=4500) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=5000) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=100) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=1800) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=2000) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=2200) # 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=2500) #  1.6min RMSE 5.98
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=2500) #  1.6min RMSE 622 
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=6000) # 23min
train.test.rbf("csv_small50", w,d,dsw,maw,sigma=1e-11,n=10000) #  MUCH MORE -> discard




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


# 20181203----------------------------------------
source("models.R")
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100),
            k=3) # RMSE 233 NICE FIT!
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=1, C=100)) # RMSE 644
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-9, e=1, C=100)) # fails misearble
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-9, e=.01, C=100)) # bad fit
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=100)) # RMSE 96

train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "rvm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-11))

# fix this
source("models.R")
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical")
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.model", "csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.rda", "csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.rda", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")

# internal testing
datafile <- "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata"
model.func.string <- "svm.rbf"
params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100)

modelfilename <- "svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.model"; datafilename <-  "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata"; subsel <- 1500; xlab <-  "t in 10ms";
