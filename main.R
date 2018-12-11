rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")
source("training.R")
source("plot.R")
source("testing.R")



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
source("training.R")
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100),
            k=3) # RMSE 233
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=100),
            k=3) # RMSE 96

train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "rvm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-11))


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

train.model("csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=100)) 


train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "rvm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-11))

# fix this
source("models.R")
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical")
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="numerical")
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="numerical")



test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.model", "csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.rda", "csv_small50_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.rda", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms")

# internal testing
datafile <- "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata"
model.func.string <- "svm.rbf"
params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100)

modelfilename <- "svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms.model"; datafilename <-  "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata"; subsel <- 1500; xlab <-  "t in 10ms";


# 20181205----------------------------------------
source("models.R")
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100),
            k=3) # RMSE 233
train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=100),
            k=3) # RMSE 96
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical")
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="numerical")

train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "rvm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-11),
            k=3)
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical")

train.model("csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
            "rvm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-06),
            k=3)
test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model", "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical")



#20181206---PLOTTING-----------------------------------
source("training.R")

train.model("csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=10,maw=250,n=1500,sigma=1e-7, e=.1, C=100),
            k=3) # RMSE 233
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 1000, "t in 10ms", test.type="graphical", error_data_raw=TRUE)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 1500, "t in 10ms", test.type="graphical", error_data_raw=TRUE)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 3500, "t in 10ms", test.type="graphical", error_data_raw=FALSE)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 2500, "t in 10ms", test.type="graphical", error_data_raw=TRUE)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 600, "t in 10ms", test.type="graphical", error_data_raw=TRUE)



# test.model("rvm.rbf__n_1500_sigma_1e-11_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model","csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 1000, "t in 10ms", test.type="graphical", error_data_raw=TRUE)


# less aggressive downsampling test
source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")
source("training.R")
#data.preparation("csv_small5",w=500,d=100,10,50)
train.model("csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw50.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=10,maw=50,n=1500,sigma=1e-7, e=.1, C=100),
            k=3)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw50.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw50.Rdata", 3050, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # close to smoothed error
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw50.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw50.Rdata", 3050, "t in 10ms", test.type="graphical", error_data_raw=TRUE) # sstill far from raw error

#data.preparation("csv_small5",w=500,d=100,1,10)
train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-7, e=.1, C=100),
            k=3)
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           20000, "t in 10ms", test.type="graphical", error_data_raw=TRUE) 
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # still the model is not fitting a lot -> decrease the sigma?

train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-9, e=.1, C=100),
            k=3)
test.model("svm.rbf__n_1500_sigma_1e-09_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # KO super bad model

train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-7, e=.01, C=100),
            k=3) # rmse 4495
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.01_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) 


train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-7, e=.1, C=10),
            k=3) # C =10 is bad, RMSE 5023
train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-6, e=.1, C=100),
            k=3) # 1e-6   RMSE 3931
train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-7, e=.01, C=10),
            k=3) # very bad 

train.model("csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-5, e=.1, C=1000),
            k=3)

## downsampling comparison -----
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.model", "csv_small5_w500ms_d100ms_wp200_dp40_dsw10_maw250.Rdata", 1750, "t in 10ms", test.type="graphical", error_data_raw=FALSE)  # too averaged
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # a bit better
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.01_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # similar to e=.1

#----sigma,e,C comparison ----
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # baseline for now
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1168___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.model",
           "csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250.Rdata",
           750, "t in 10ms", test.type="graphical", error_data_raw=TRUE) # model 2 in report
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.01_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # similar to e=.1
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.1_C_10_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE)  # BAD
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # MUCH better!!!
test.model("svm.rbf__n_1500_sigma_1e-07_e_0.01_C_10_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE)  # BAD
test.model("svm.rbf__n_1500_sigma_1e-05_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=FALSE) # very good, maybe too much model 4 on report
test.model("svm.rbf__n_1500_sigma_1e-05_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp2000_dp400_dsw1_maw10.Rdata",
           30000, "t in 10ms", test.type="graphical", error_data_raw=TRUE) # same as before but with noise target(no downsampling)


# prepare datasets -----------------------------------
source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")
source("training.R")
source("plot.R")
source("testing.R")
data.preparation("csv_small5",w=1000,d=100,10,250)
data.preparation("csv_small5",w=500,d=100,1,10)
data.preparation("csv_small5",w=500,d=100,1,5)
data.preparation("csv_small5",w=500,d=100,1,1)



# downsampling study
study.ma.values()



# 20181210 prepare datasets -----------------------------------
source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")
source("training.R")
source("plot.R")
source("testing.R")
# data.preparation("csv_small5",w=1000,d=100,10,250)
# data.preparation("csv_small5",w=500,d=100,1,10)
# data.preparation("csv_small5",w=500,d=100,1,5)
# data.preparation("csv_small5",w=500,d=100,1,1)
train.model("csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-6, e=.1, C=1000),
            k=3) 
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
           4000,  # i ms
           "t in 10ms", 
           test.type="graphical", 
           error_data_raw=TRUE,
           start.point=2) # in ms 
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
           8000,  # i ms
           "t in 10ms", 
           test.type="graphical", 
           error_data_raw=TRUE,
           start.point=1) # in ms


# testing sop.plot with x lab in ms instead of datapoints
train.model("csv_small5_w1000ms_d100ms_wp100_dp10_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=1000),
            k=3) 
source("plot.R")
source("testing.R")
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__344___404_csv_small5_w1000ms_d100ms_wp100_dp10_dsw10_maw250.model",
           "csv_small5_w1000ms_d100ms_wp100_dp10_dsw10_maw250.Rdata",
           8000,  # i ms
           "t in 10ms", 
           test.type="graphical", 
           error_data_raw=TRUE,
           start.point=1) # in ms


source("preprocessing_downsampling.R")
source("preprocessing_plot.R")
source("preprocessing_features.R")
source("preprocessing_readcsv.R")
source("models.R")
source("training.R")
source("plot.R")
source("testing.R")
data.preparation("csv_small5",w=1000,d=100,10,250)

data.preparation("csv_small5",w=1000,d=10,10,250)
train.model("csv_small5_w1000ms_d10ms_wp100_dp1_dsw10_maw250.Rdata",
            "svm.rbf",
            params=list(w=1000,d=10,dsw=10,maw=250,n=1500,sigma=1e-6, e=.1, C=1000),
            k=3) 
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__344___404_csv_small5_w1000ms_d10ms_wp100_dp1_dsw10_maw250.model",
           "csv_small5_w1000ms_d10ms_wp100_dp1_dsw10_maw250.Rdata",
           8000,  # i ms
           "t in 10ms", 
           test.type="graphical", 
           error_data_raw=TRUE,
           start.point=1) # in ms

data.preparation("csv_small5",w=500,d=100,1,10)
train.model("csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
            "svm.rbf",
            params=list(w=500,d=100,dsw=1,maw=10,n=1500,sigma=1e-6, e=.1, C=1000),
            k=3) 
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
           8000,  # i ms
           "t in 10ms", 
           test.type="graphical", 
           error_data_raw=TRUE,
           start.point=1) # in ms 

#-------------------------------
test.model("svm.rbf__n_1500_sigma_1e-06_e_0.1_C_1000_dim__1704___2004_csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.model",
           "csv_small5_w500ms_d100ms_wp500_dp100_dsw1_maw10.Rdata",
           1000, "t in 10ms", test.type="graphical", error_data_raw=TRUE,start.point=1) 

# fix data.preparation
data.preparation("csv_all",w=500,d=100,1,1)
data.preparation("csv_all",w=500,d=100,1,5)
data.preparation("csv_all",w=500,d=100,1,10)
data.preparation("csv_all",w=500,d=100,1,50)