my.load.file <- function(datafile,params){
  filename = paste("./data/preprocessed/",datafile,sep="")
  load(file = filename)
  if ("n" %in% params){
    n = params["n"][[1]]
  } else n = nrow(df3)
  subselect <- sample(1:nrow(df3),n)
  df <- df3[subselect,]
}

prepare.XY.data <- function(df,params){
  
  # select columns
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  X <- df[,-remove.cols]
  Y <- df[,grep("^futurey$", colnames(df))]
  
  # sample: train dataset, validation dataset, testing dataset
  if ("train.props" %in% params){
    train.prop = params["train.props"][[1]]
  } else train.prop = 0.6
  
  if ("validation.props" %in% params){
    val.prop = params["validation.props"][[1]]/(1-train.prop)
  } else val.prop = 0.2/(1-train.prop) # 50% of the rest of data after removing train.prop
  
  if ("testing.props" %in% params){
    test.prop = params["testing.props"][[1]]/(1-train.prop-val.prop*(1-train.prop))
  } else test.prop = 0.2/(1-train.prop-val.prop*(1-train.prop)) # 100% of the rest of data after removing train.prop and val.prop
  
  
  all <- seq(1:length(Y))
  train <- sample(all, length(Y)*train.prop)
  validation <- sample(all[-train], length(all[-train])*val.prop)
  test <- sample(all[-c(train,validation)], length(all[-c(train,validation)])*test.prop)
  xtrain <- as.matrix(X[train,])
  ytrain <- as.matrix(Y[train])
  xvalidation <- as.matrix(X[validation,])
  yvalidation <- as.matrix(Y[validation])
  xtest <- as.matrix(X[test,])
  ytest <- as.matrix(Y[test])
  
  return(list(xtrain,ytrain,xvalidation,yvalidation,xtest,ytest))
}

get.func.name <- function(f1){
  deparse(substitute(f1))
}

get.string.params <- function(params) {
  '%ni%' <- Negate('%in%')
  paramstring <- ""
  for(i in names(params)){
    if (i %ni% c("w","d","dsw","maw")) {
      
      paramstring <- paste(paramstring,i,as.character(params[i][[1]]),sep="_", collapse="")
    }
  }
  return(paramstring)
}

save.model <- function(model, modelfunc, datafile, params, xtrain, ytrain){
  
  w = params["w"][[1]]
  d = params["d"][[1]]
  dsw = params["dsw"][[1]]
  maw = params["maw"][[1]]
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  
  #modelstring = get.func.name(modelfunc) # not working
  modelstring = modelfunc
  paramstring = get.string.params(params)
  datafile2 = substr(datafile,1,nchar(datafile)-6+1)
  
  filename_sub = paste(modelstring,paramstring,"dim_",dim(xtrain)[1],"_",dim(xtrain)[2],
                   datafile2,paste(w,"ms",sep=""),paste(d,"ms",sep=""),
                   wp,dp,paste(dsw,"ms",sep=""),paste(maw,"ms",sep=""),
                   sep="_")
  filename = paste("./models/",filename_sub,".model",sep="")
  print("saving to ")
  print(filename)
  save(model, file = filename) 
  return(filename_sub)
}

test.model.internal <- function(model,xvalidation,yvalidation ){
  #predict
  yvalidation.pred <- predict(model, xvalidation)
  # sum of squares error?
  rmse.validation = sqrt(sum((yvalidation - yvalidation.pred)^2)/nrow(xvalidation))
  return(list(pred=yvalidation.pred, rmse=rmse.validation))
}

plot.and.save.results <- function(xtrain, ytrain, xvalidation, yvalidation,yvalidation.pred, modelstring, total.time, rmse.validation){
  
  # output
  rmse = round(rmse.validation,3)
  # parse time correctly 
  timetext = format(total.time, format ="%Y-%m-%d %H:%M:%S")
  timetext = substr(timetext,1,nchar(timetext))
  i1 = regexpr('\\.',timetext)
  i2 = regexpr('\ ',timetext)
  timenum = round(as.numeric(substr(timetext,1,i2-1)),2)
  timetext = substr(timetext,i2,nchar(timetext))
  final.time.text = paste(timenum,timetext,sep="",collapse="")
  
  model.result <- paste(modelstring,final.time.text,"rmse",rmse,sep="_",collapse="")
  
  print(model.result)
  write(model.result, 
        file="results/results.txt",append=TRUE)
  # plot predict vs ground-truth
  par(mfrow=c(1,1))
  ylimmax = max(max(yvalidation),max(yvalidation.pred))
  ylimmin = min(min(yvalidation),min(yvalidation.pred))
  
  plotname = paste(c('./plots/',model.result,'.jpeg'),sep="",collapse="")
  plotname = gsub(":","_",plotname)
  plotname = gsub(" ","_",plotname)
  plotname = gsub("=","_",plotname)
  plotname = gsub(",","-",plotname)
  jpeg(plotname)
  plot(
    yvalidation,type="l", 
    main=paste("Training results",sep="",collapse=""),
    xlab=paste(model.result,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(yvalidation.pred, col="red")
  dev.off()
  
  plot(
    yvalidation,type="l", 
    main=paste("Training results",sep="",collapse=""),
    xlab=paste(model.result,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(yvalidation.pred, col="red")
}


model.functions <- list("rvm.rbf" = rvm.rbf, "svm.rbf" = svm.rbf)

train.model <- function(datafile, model.func.string, params){
  ## datafile is a preprocessed dataset in Rdata format
  ## modelfunc is a function to train SVM or RVM model
  ## params is a list that contains key values for the model to be trained
  ##      n: size of the data to extract/sample from data file
  ##      w: ms back time window
  ##      d: ms future perdict window
  ##      dsw: downsampling window in ms
  ##      maW: moving avg window in ms
  ##     (RVM rbf): sigma 
  ##     (RVM poly): c, q, n
  ##     (SVM rbf): C, e, sigma
  ##     (nu-SVM rbf): C, nu, sigma
  ##     ...
  ## example call:
  ## train.model("csv_small5_w1000ms_d100ms_wp40_dp40_dsw10_maw250.Rdata",
  ##              rvm.rbf,
  ##              params=list(w=1000,d=100,dsw=10,maw=250,n=1500,sigma=1e-11))
  
  # general params
  print("getting general params")
  for (i in names(params)){ print(i)}
  w = params["w"][[1]]
  d = params["d"][[1]]
  dsw = params["dsw"][[1]]
  maw = params["maw"][[1]]
  # translate from ms window to data points
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  
  # load data file
  print("loading data file into df")
  df <- my.load.file(datafile, params)

  # prepare X,Y 
  print("subseting X,Y into train,val, test")
  subsets <- prepare.XY.data(df, params)
  xtrain <- subsets[[1]]; ytrain<- subsets[[2]]; 
  xvalidation <- subsets[[3]]; yvalidation<- subsets[[4]];
  xtest <- subsets[[5]]; ytest <- subsets[[6]];
  
  # train model & save
  print("training model")
  start_time <- Sys.time()
  modelfunc <- model.functions[model.func.string][[1]]
  model <- modelfunc(xtrain,ytrain, params)
  if (is.null(model)) return(NULL) # stop if there was a problem training model
  end_time <- Sys.time()
  total.time = end_time - start_time
  print(total.time)
  
  # save model
  print("saving model")
  modelstring <- save.model(model, model.func.string, datafile, params ,xtrain, ytrain)
  
  # cross-validation using RMSE
  # save model
  # save cv results
  print("validation( for now simple prediction)")
  predict.result <- test.model.internal(model, xvalidation, yvalidation)
  rmse <- predict.result["rmse"][[1]]
  yvalidation.pred <- predict.result["pred"][[1]]
  
  # save plot 
  print("plotting")
  plot.and.save.results(xtrain, ytrain, xvalidation, yvalidation,yvalidation.pred, modelstring, total.time, rmse)
  
  # testing cv -> RMSE
  
  
}


test.model <- function(modelfilename, datafilename, subselect, xlab){
  
  # modelfilename = "csv_small5_1000ms_100ms_400_40_10_250_rbf_sigma_1e-11_model.rda"
  # datafilename = "csv_small75_w100ms_d100ms_wp40_dp40_dsw10_maw250.Rdata"
  # subselect = 1500
  # xlab = "t in 10ms"
  
  # ,xtest, ytest
  load(file = paste("./data/preprocessed/",datafilename,sep="") ) # output is df3
  subselect <- sample(1:nrow(df3),subselect)
  df3 <- df3[100,]
  # if nrow(df3)<subselect -> pad with same data
  df <- df3[1,]
  if (nrow(df3) < subselect){  
  for (i in 2:subselect){
      df <- rbind(df,df3[i,])
    }
  } else df <- df3[subselect,]
  
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  xtest <- df[,-remove.cols]
  ytest <- df[,grep("^futurey$", colnames(df))]
  
  # load model
  load(file = paste("./models/",modelfilename,sep="")) # output to "model" object
  #predict
  if (class(model)[1] == "ksvm"){
    library(kernlab)
    ytest.pred <- predict(object = model, newdata = xtest, type="response")
  } else ytest.pred <- predict(model, xtest)
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

rvm.rbf <- function(xtrain, ytrain, params){
  print("calling rvm.rbf")
  
  if ("sigma" %in% names(params) ){
    library(kernlab)
    model <- rvm(xtrain,ytrain,
                 type = "regression",
                 kernel = "rbfdot",
                 kpar = list(sigma=params["sigma"][[1]]),
                 verbosity = 2)  
    return(model)
    } else return(NULL)
}


svm.rbf.1 <- function(xtrain, ytrain, params){
  print("calling svm.rbf")
  
  if ("sigma" %in% names(params) && "C" %in% names(params) && "e" %in% names(params) ){
    library(e1071)
      
    e = params["e"][[1]]
    C = params["C"][[1]]
    sigma = params["sigma"][[1]]
    model <- svm(ytrain ~ .,xtrain, cost=C[1], epsilon=e[1], type="eps-regression", kernel="rbf", gamma=1/sigma )
    #tuneResult <- tune(svm, Y ~ X,  data = data,
    #                   ranges = list(epsilon = seq(e[1],e[2],e[3]), cost = 2^(C[1]:C[2])))
    return(model)
  } else return(NULL)
  
}

svm.rbf <- function(xtrain, ytrain, params){
  print("calling svm.rbf ")
  
  if ("sigma" %in% names(params) && "C" %in% names(params) && "e" %in% names(params) ){
    library(kernlab)
    
    e = params["e"][[1]]
    C = params["C"][[1]]
    sigma = params["sigma"][[1]]
    model <- ksvm(xtrain, ytrain,kernel="rbfdot" , C=C[1], epsilon=e[1], type="eps-svr",  kpar=list(sigma=sigma) )
    #print(model)
    #print(class(model))
    save(model, file="ksvm.model")
    return(model)
  } else return(NULL)
  
}

