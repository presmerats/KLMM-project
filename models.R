train.test.pre <- function(fileprefix,w,d,dsw,maw, n){
  
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  
  
  # load file
  filename = paste(fileprefix,paste("w",w,"ms",sep=""),paste("d",d,"ms",sep=""),paste("wp",wp,sep=""),paste("dp",dp,sep=""),paste("dsw",dsw,sep=""),paste("maw",maw,sep=""),sep="_")
  filename = paste("./data/preprocessed/",filename,".Rdata",sep="")
  load(file = filename)
  subselect <- sample(1:nrow(df3),n)
  df <- df3[subselect,]
  # prepare X,Y 
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  X <- df[,-remove.cols]
  Y <- df[,grep("^futurey$", colnames(df))]
  # sample: train dataset, validation dataset, testing dataset
  train.prop = 0.6
  val.prop = 0.5
  test.prop = 1
  all <- c(1:length(Y))
  train <- sample(all, length(Y)*train.prop)
  validation <- sample(all[-train], length(all[-train])*val.prop)
  test <- sample(all[-c(train,validation)], length(all[-c(train,validation)])*test.prop)
  xtrain <- as.matrix(X[train,])
  ytrain <- as.matrix(Y[train])
  xvalidation <- as.matrix(X[validation,])
  yvalidation <- as.matrix(Y[validation])
  xtest <- as.matrix(X[test,])
  ytest <- as.matrix(Y[test])
  
  return(list(xtrain, ytrain, xvalidation, yvalidation, xtest, ytest))
  
}




test.model <- function(modelfilename, datafilename, subselect, xlab){
  
  # modelfilename = "csv_small5_1000ms_100ms_400_40_10_250_rbf_sigma_1e-11_model.rda"
  # datafilename = "csv_small75_w100ms_d100ms_wp40_dp40_dsw10_maw250.Rdata"
  # subselect = 1500
  # xlab = "t in 10ms"
  
  # ,xtest, ytest
  load(file = paste("./data/preprocessed/",datafilename,sep="") ) # output is df3
  subselect <- sample(1:nrow(df3),subselect)
  df <- df3[subselect,]
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  xtest <- df[,-remove.cols]
  ytest <- df[,grep("^futurey$", colnames(df))]
  
  # load model
  load(file = paste("./models/",modelfilename,sep="")) # output to "model" object
  #predict
  ytest.pred <- predict(model, xtest)
  # sum of squares error?
  rmse.test = sqrt(sum((ytest - ytest.pred)^2)/nrow(xtest))
  
  # output
  rmse.plot = round(rmse.test,3)
  
  
  
  # plot
  par(mfrow=c(1,1))
  ylimmax = max(max(ytest),max(ytest.pred))
  ylimmin = min(min(ytest),min(ytest.pred))
  plotname= paste("./plots/","test",modelfilename,datafilename,paste("RMSE",rmse.plot,sep=""), sep="_" )
  jpeg(plotname)
  plot(
    ytest,type="l", 
    main=paste(plotname,sep="",collapse=""),
    xlab=paste(xlab,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(ytest.pred, col="red")
  dev.off()
  
}

train.test.post <- function(model, 
                            xvalidation, yvalidation, 
                            output.kv, plot.title, plot.xlab,
                            start_time){
  #predict
  yvalidation.pred <- predict(model, xvalidation)
  # sum of squares error?
  rmse.validation = sqrt(sum((yvalidation - yvalidation.pred)^2)/nrow(xvalidation))
  # total time
  end_time <- Sys.time()
  total.time = end_time - start_time
  print("total time training")
  print(total.time)
  
  # output
  output.kv[length(output.kv)] = round(rmse.validation,3)
  plot.xlab[length(plot.xlab)] = round(rmse.validation,3)
  output.kv[length(output.kv)-2] = format(total.time, format ="%Y-%m-%d %H:%M:%S")
  plot.xlab[length(plot.xlab)-2] = format(total.time, format ="%Y-%m-%d %H:%M:%S")
  
  # parse time correctly 
  timetext = output.kv[length(output.kv)-2]
  timetext = substr(timetext,1,nchar(timetext))
  i1 = regexpr('\\.',timetext)
  i2 = regexpr('\ ',timetext)
  timenum = round(as.numeric(substr(timetext,1,i2-1)),2)
  timetext = substr(timetext,i2,nchar(timetext))
  output.kv[length(output.kv)-2] = paste(timenum,timetext,sep="",collapse="")
  
  print(paste(output.kv,sep="",collapse=""))
  write(paste(output.kv,sep="",collapse=""), 
        file="results/results.txt",append=TRUE)
  # plot predict vs ground-truth
  par(mfrow=c(1,1))
  ylimmax = max(max(yvalidation),max(yvalidation.pred))
  ylimmin = min(min(yvalidation),min(yvalidation.pred))
  
  plotnamevec = output.kv
  plotnamevec[length(plotnamevec)] = round(as.numeric(plotnamevec[length(plotnamevec)]),2)
  timetext = plotnamevec[length(plotnamevec)-2]
  timetext = substr(timetext,1,nchar(timetext))
  i1 = regexpr('\\.',timetext)
  i2 = regexpr('\ ',timetext)
  timenum = round(as.numeric(substr(timetext,1,i2-1)),0)
  timetext = substr(timetext,i2,nchar(timetext))
  plotnamevec[length(plotnamevec)-2] = paste(timenum,timetext,sep="",collapse="")
  plotname = paste(c('./plots/',plotnamevec,'.jpeg'),sep="",collapse="")
  plotname = gsub(":","_",plotname)
  plotname = gsub(" ","_",plotname)
  plotname = gsub("=","_",plotname)
  plotname = gsub(",","-",plotname)
  jpeg(plotname)
  plot(
    yvalidation,type="l", 
    main=paste(plot.title,sep="",collapse=""),
    xlab=paste(plot.xlab,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(yvalidation.pred, col="red")
  dev.off()
  
}


train.test.vanilladot <- function(fileprefix,w,d,dsw,maw){
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  #paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp
  
  #return(list(xtrain, ytrain, xvalidation, yvalidation, xtest, ytest))
  pre <- train.test.pre(fileprefix, w,d,dsw,maw)
  xtrain <- pre[[1]]
  ytrain <- pre[[2]]
  xvalidation <- pre[[3]]
  yvalidation <- pre[[4]]
  xtest <- pre[[5]]
  ytest <- pre[[6]]
  # time
  start_time <- Sys.time()
  # training
  library(kernlab)
  model <- rvm(xtrain,ytrain,
               type = "regression",
               kernel = "vanilladot",
               verbosity = 2)
  # save model
  filename = paste(fileprefix,paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp,dsw,maw,sep="_")
  filename = paste("./models/",filename,"vanilladot", "_model.rda",sep="")
  save(model, file = filename) 
  # prepare outputs
  modelstring = "Model:vanilladot"
  output.kv = c(modelstring,paste(fileprefix," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw,
                " dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " training:","",
                " RMSE=","")
  plot.title = c("RMSE  ",paste(modelstring," ",fileprefix," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw)
  plot.xlab = c(" dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " training:","",
                " RMSE=","") 
  # inputs model, xvalidation, yvalidation, output.kv, plot.title, plot.xlab, start_time
  train.test.post(model, xvalidation, yvalidation, output.kv,plot.title, plot.xlab, start_time )
}

train.test.auto <- function(fileprefix,w,d,dsw,maw){
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  #paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp
  
  
  #return(list(xtrain, ytrain, xvalidation, yvalidation, xtest, ytest))
  pre <- train.test.pre(fileprefix, w,d,dsw,maw)
  xtrain <- pre[[1]]
  ytrain <- pre[[2]]
  xvalidation <- pre[[3]]
  yvalidation <- pre[[4]]
  xtest <- pre[[5]]
  ytest <- pre[[6]]
  # time
  start_time <- Sys.time()
  # training
  library(kernlab)
  model <- rvm(xtrain,ytrain,
               verbosity = 2)
  # save model
  filename = paste(fileprefix,"auto",paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp,dsw,maw,sep="_")
  filename = paste("./models/",filename,"auto","_model.rda",sep="")
  save(model, file = filename) 
  # prepare outputs
  modelstring = "rbfdot_auto"
  output.kv = c("Model:",modelstring,paste(fileprefix," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw,
                " dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " training:","",
                " RMSE=","")
  plot.title = c("RMSE  ",paste(modelstring," ",fileprefix,," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw)
  plot.xlab = c(" dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " training:","",
                " RMSE=","") 
  # inputs model, xvalidation, yvalidation, output.kv, plot.title, plot.xlab, start_time
  train.test.post(model, xvalidation, yvalidation, output.kv,plot.title, plot.xlab, start_time )
  
}

train.test.rbf <- function(fileprefix,w,d,dsw,maw,
                           sigma=1,n){
  
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  #paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp
  
  
  #return(list(xtrain, ytrain, xvalidation, yvalidation, xtest, ytest))
  pre <- train.test.pre(fileprefix, w,d,dsw,maw,n)
  xtrain <- pre[[1]]
  ytrain <- pre[[2]]
  xvalidation <- pre[[3]]
  yvalidation <- pre[[4]]
  xtest <- pre[[5]]
  ytest <- pre[[6]]
  # time
  start_time <- Sys.time()
  # training
  library(kernlab)
  model <- rvm(xtrain,ytrain,
               type = "regression",
               kernel = "rbfdot",
               kpar = list(sigma=sigma),
               verbosity = 2)
  # save model
  modelstring = "rbf"
  filename = paste(fileprefix,paste(w,"ms",sep=""),paste(d,"ms",sep=""),wp,dp,dsw,maw,sep="_")
  filename = paste("./models/",filename,"_rbf","_sigma_",sigma,"_model.rda",sep="")
  save(model, file = filename) 
  # prepare outputs
  output.kv = c("Model:",modelstring,paste(fileprefix," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw,
                " dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " params ","sigma:",sigma,
                " training:","",
                " RMSE=","")
  plot.title = c("RMSE  ",paste(modelstring," ",fileprefix," sigma:",sigma," w:",w,"ms ",sep=""),paste("d:",d,"ms",sep="")," wp:",wp," dp:",dp," dsw:",dsw," ma:",maw)
  plot.xlab = c(" dim:",dim(xtrain)[1],",",dim(xtrain)[2],
                " training:","",
                " RMSE=","") 
  # inputs model, xvalidation, yvalidation, output.kv, plot.title, plot.xlab, start_time
  train.test.post(model, xvalidation, yvalidation, output.kv,plot.title, plot.xlab, start_time )
  
}
