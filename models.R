

my.load.file <- function(datafile,params){
  filename = paste("./data/preprocessed/",datafile,sep="")
  load(file = filename)
  if ("n" %in% params){
    n = params["n"][[1]]
  } else n = nrow(df3)
  subselect <- sample(1:nrow(df3),n)
  df <- df3[subselect,]
}

prepare.XY.data <- function(df,params,k="1"){
  
  # select columns
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  X <- df[,-remove.cols]
  Y <- df[,grep("^futurey$", colnames(df))]
  
  # dimensionality
  maxrows <- ncol(X)*0.85
  
  
  # sample: train dataset, validation dataset, testing dataset
  if ("train.props" %in% params){
    train.prop = params["train.props"][[1]]
  } else train.prop = 0.7
  
  # this makes no much sense with k-fold cross-validation
  # if ("validation.props" %in% params){
  #   val.prop = params["validation.props"][[1]]/(1-train.prop)
  # } else val.prop = 0.2/(1-train.prop) # 50% of the rest of data after removing train.prop
  # 
  # if ("testing.props" %in% params){
  #   test.prop = params["testing.props"][[1]]/(1-train.prop-val.prop*(1-train.prop))
  # } else test.prop = 0.3/(1-train.prop-val.prop*(1-train.prop)) # 100% of the rest of data after removing train.prop and val.prop
  
  
  all <- seq(1:length(Y))
  # xtrain is a list of length k of different arrays
  sampled.train <- sample(all, length(Y)*train.prop)
  # just for testing and verification of the k-fold crossvalidation sampling
  # sampled.train <- all[1:(length(Y)*train.prop)]
  
  train.folds <- list()
  fold.length <- length(sampled.train)/k
  print(paste(" total length",length(Y)," total length*train.prop",length(Y)*train.prop," fold length", fold.length, " training (k-1) folds length",fold.length*(k-1)," maxrows for training",maxrows))
  # training dataset size verification
  if (fold.length*(k-1) > maxrows){
    print(paste(" Error dimensions...",fold.length*(k-1)," > ",maxrows,"(max rows)"))
    length.train <- maxrows*k/(k-1)
    new.train.prop <- length.train/length(Y)
    print(paste(" recomputing k-folds to: ",length.train," which is a proportion of ",new.train.prop," of all data"))

    sampled.train <- sample(all, length(Y)*new.train.prop)
    # just for testing
    # sampled.train <- all[1:(length(Y)*new.train.prop)]
    
    fold.length <- length(sampled.train)/k
  }
  
  
  # divide sampled.train in k su<barrays of length l/k
  for (i in 1:k){
    # train.fold is an array of length fold.length with a subset of the sampled.train
    train.fold <- sampled.train[((i-1)*fold.length):(i*fold.length)]
    #list.append(train.folds , train.fold)
    train.folds[[i]] <- train.fold
    #print(paste(i,"-fold: ",train.fold[1],"-",train.fold[length(train.fold)]," length:",length(train.fold),sep=""))
  }
  
  train.list <- list()
  validation.list <- list()
  for (i in 1:k){
    # get ktrain and kvalidation
    kvalidation <- train.folds[i][[1]]
    # ktrain gets a concatenation of all other lists excep the one at i
    ktrain <- train.folds[-i][[1]]
    for (j in 2:length(train.folds[-i])){
      ktrain <- c(ktrain, train.folds[-i][[j]])  
    }
    #print(paste(i,"-fold train: ",ktrain[1],"-",ktrain[length(ktrain)]," length:",length(ktrain),sep=""))
    #print(paste(i,"-fold validation: ",kvalidation[1],"-",kvalidation[length(kvalidation)]," length:",length(kvalidation),sep=""))
    validation.list[[i]] <-  kvalidation
    train.list[[i]] <- ktrain
  }
  # test is just a single array inside a list
  test <- list(all[-sampled.train])
  
  return(list(train.list, validation.list ,test,X,Y))
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

save.model <- function(model, modelfunc, datafile, params, dim1x, dim2x){
  
  w = params["w"][[1]]
  d = params["d"][[1]]
  dsw = params["dsw"][[1]]
  maw = params["maw"][[1]]
  wp=round(w/0.25/dsw,0)
  dp=round(d/0.25/dsw,0)
  
  #modelstring = get.func.name(modelfunc) # not working
  modelstring = modelfunc
  paramstring = get.string.params(params)
  print(paste("paramstrings",paramstring,sep=" "))
  datafile2 = substr(datafile,1,nchar(datafile)-6)
  #print(datafile2)
  
  filename_sub = paste(modelstring,paramstring,"dim_",dim1x,"_",dim2x,
                   datafile2,
                   # instead do a verification that w,d,dsw,maw are the same as in the datafile name
                   #,paste(w,"ms",sep=""),paste(d,"ms",sep=""),
                   # wp,dp,paste(dsw,"ms",sep=""),paste(maw,"ms",sep=""),
                   sep="_")
  filename = paste("./models/",filename_sub,".model",sep="")
  print("saving to ")
  print(filename)
  save(model, file = filename) 
  return(filename_sub)
}

test.model.internal <- function(model,xvalidation,yvalidation, loss="rmse" ){
  #predict
  yvalidation.pred <- predict(model, xvalidation)
  # sum of squares error?
  if (loss=="rmse"){
    rmse.validation = sqrt(2*sum((yvalidation - yvalidation.pred)^2)/nrow(xvalidation))
    return(list(pred=yvalidation.pred, rmse=rmse.validation))
    
  } else if (loss=="se"){
    se.validation = sum((yvalidation - yvalidation.pred)^2)
    return(list(pred=yvalidation.pred, rmse=rmse.validation))
  }
}

plot.and.save.results <- function( xvalidation, yvalidation,yvalidation.pred, modelstring, total.time, rmse.validation){
  
  # output
  rmse = round(rmse.validation,3)
  # parse time correctly 
  timetext = format(total.time, format ="%Y-%m-%d %H:%M:%S")
  timetext = substr(timetext,1,nchar(timetext))
  i1 = regexpr('\\.',timetext)
  i2 = regexpr('\ ',timetext)
  timenum = round(as.numeric(substr(timetext,1,i2-1)),2)
  timetext = substr(timetext,i2+1,nchar(timetext))
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
  jpeg(plotname, width = 480, height = 480, units = "px")
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


verify.dataset <- function(datafile, w,d,dsw,maw,wp,dp){
  

  # extract w,d,dsw,maw,wp,dp from name
  i1 = regexpr('_',datafile)
  aux = substr(datafile,i1+1,nchar(datafile))
  i2 = regexpr('_',aux)
  aux = substr(aux,i2+1,nchar(aux))
  i3 = regexpr('_',aux)
  aux = substr(aux,i3+1,nchar(aux))
  i4 = regexpr('_',aux)
  aux = substr(aux,i4+1,nchar(aux))
  i5 = regexpr('_',aux)
  aux = substr(aux,i5+1,nchar(aux))
  i6 = regexpr('_',aux)
  aux = substr(aux,i6+1,nchar(aux))
  i7 = regexpr('_',aux)
  aux = substr(aux,i7+1,nchar(aux))
  i8 = regexpr('\\.',aux)
  aux = substr(aux,i8+1,nchar(aux))
  
  w2 = as.numeric(substring(datafile,i1+i2+2,i1+i2+i3-3))
  d2 = as.numeric(substring(datafile,i1+i2+i3+2,i1+i2+i3+i4-3))
  wp2 = as.numeric(substring(datafile,i1+i2+i3+i4+3,i1+i2+i3+i4+i5-1))
  dp2 = as.numeric(substring(datafile,i1+i2+i3+i4+i5+3,i1+i2+i3+i4+i5+i6-1))
  dsw2 = as.numeric(substring(datafile,i1+i2+i3+i4+i5+i6+4,i1+i2+i3+i4+i5+i6+i7-1))
  maw2 = as.numeric(substring(datafile,i1+i2+i3+i4+i5+i6+i7+4,i1+i2+i3+i4+i5+i6+i7+i8-1))
  # print(w2)
  # print(d2)
  # print(wp2)
  # print(dp2)
  # print(dsw2)
  # print(maw2)
  
  # compare to w,d,dsw,maw,wp,dp
  if (w!=w2 || d!=d2 || wp!=wp2 || dp!=dp2  || dsw!=dsw2 || maw!=maw2){
    print("uncompatible parameters for dataset")
    return(FALSE)
  } else return(TRUE)
  
}

model.fitting <- function(modelfunc,params, X,Y,train.list, validation.list, test.list){
  modelfunc <- model.functions[model.func.string][[1]]
  
  k <- length(train.list)
  rmse.avg <- 0
  total.time.avg <- 0
  for (i in 1:k){
    start_time <- Sys.time()
    xtrain <- as.matrix(X[train.list[[i]],])
    ytrain <- as.matrix(Y[train.list[[i]]])
    xvalidation <- as.matrix(X[validation.list[[i]],])
    yvalidation <- as.matrix(Y[validation.list[[i]]])
    
    # fit model
    model <- modelfunc(xtrain,ytrain, params)
    if (is.null(model)) return(NULL) # stop if there was a problem training model
    
    # compute rmse
    res <- test.model.internal(model, xvalidation, yvalidation, loss="rmse")
    rmse <- res[[2]]
    ypred <- res[[1]]
    rmse.avg <- rmse.avg + rmse
    
    end_time <- Sys.time()
    total.time = end_time - start_time
    total.time.avg <- total.time.avg + total.time
    
    print(paste(" i=",i," rmse=",rmse," training time: ",total.time))
    
  }
  rmse.avg <- rmse.avg/k
  total.time.avg <- total.time.avg/k
  print(paste(" model rmse.avg: ",rmse.avg," avg training time",total.time.avg))
  
  # prepare the final model
  xtotaltrain <- as.matrix(X[c(train.list[[i]],validation.list[[i]]),])
  ytotaltrain <- as.matrix(Y[c(train.list[[i]],validation.list[[i]])])
  model <- modelfunc(xtotaltrain,ytotaltrain, params)
  

  return(list(model, dim(X[train.list[[1]]]), rmse.avg, total.time))
}
  

train.model <- function(datafile, model.func.string, params,k=3){
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
  #for (i in names(params)){ print(i)}
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
  
  #verify params compatible with dataset
  compatible <- verify.dataset(datafile, w,d,dsw,maw,wp,dp)
  if (compatible == FALSE) return(NULL)
  
  # prepare X,Y 
  print("subseting X,Y into train,val, test")
  subsets <- prepare.XY.data(df, params,k)
  train.list <- subsets[[1]]
  validation.list <- subsets[[2]];
  test.list <- subsets[[3]]
  X <- subsets[[4]]
  Y <- subsets[[5]]
  
  # train model with cross-validation
  print("training model")
  fitting.results <- model.fitting(modelfunc,params, X,Y,train.list, validation.list, test.list)
  model <- fitting.results[[1]]
  dimensions <- fitting.results[[2]]
  rmse.avg <- fitting.results[[3]]
  total.time <- fitting.results[[4]]
  
  # save model
  print("saving model")
  modelstring <- save.model(model, model.func.string, datafile, params ,dimensions[1], dimensions[2])
  
  # save plot 
  print("plotting")
  # just for plotting
  xtrain <- X[train.list[[1]],]
  ytrain <- Y[train.list[[1]]]
  xvalidation <- X[validation.list[[1]],]
  yvalidation <- Y[validation.list[[1]]]
  predict.result <- test.model.internal(model, xvalidation, yvalidation)
  rmse <- predict.result["rmse"][[1]]
  yvalidation.pred <- predict.result["pred"][[1]]
  plot.and.save.results(xvalidation, yvalidation, yvalidation.pred, modelstring, total.time, rmse.avg)
  
  # testing cv -> RMSE
  predict.result <- test.model.internal(model, X[test.list[[1]],], Y[test.list[[1]]])
  rmse <- predict.result["rmse"][[1]]
  yvalidation.pred <- predict.result["pred"][[1]]
  print(paste("test rmse",rmse))
  
}


dimension.verification <- function(modelfilename, xtest){
  i1 = regexpr('_dim__',modelfilename)
  i2 = regexpr('_csv',modelfilename)
  if (i1>0 && i2>0){
    dims = substr(modelfilename,i1+7,i2-1)
    i3 = regexpr('_',dims)
    dim1 = as.numeric(substr(dims,1,i3-1))
    dim2 = as.numeric(substr(dims,i3+3,nchar(dims)))
    
    #print(dim2)
    #print(dim(xtest)[2])
    if(dim2!=dim(xtest)[2]){
      print(paste("incompatible column dimensions",dim2,dim(xtest)[2],sep=" "))
      return(FALSE)
    } else return(TRUE)
  } 
  return(TRUE)
}

plot.and.save.test.result <- function(xtest, ytest,ytest.pred,modelfilename,datafilename,rmse.plot, test.type){
  
  par(mfrow=c(1,1))
  ylimmax = max(max(ytest),max(ytest.pred))
  ylimmin = min(min(ytest),min(ytest.pred))
  # plot to disk
  print("modelfilename")
  print(modelfilename)
  print("datafilename")
  print(datafilename)
  plotname= paste("./plots/","test",modelfilename,paste("RMSE",rmse.plot,sep="_"), sep="_" )
  
  
  if (test.type=="numerical"){

      jpeg(plotname, width = 768, height = 480, units = "px")
      plot(
        ytest,type="l", 
        main=paste("Test result",sep="",collapse=""),
        xlab=paste(plotname,sep="",collapse=""),
        ylab="BER",
        ylim=c(ylimmin,ylimmax))
      lines(ytest.pred, col="red")
      legend(1, ylimmax-2, legend=c("Real Error", "Predicted Error"),
             col=c("black", "red"), lty=1:1, cex=0.8)
      dev.off()
      
      # plot in r studio
      plot(
        ytest,type="l", 
        main=paste("Test result",sep="",collapse=""),
        xlab=paste(plotname,sep="",collapse=""),
        ylab="BER",
        ylim=c(ylimmin,ylimmax))
      lines(ytest.pred, col="red")
      legend(1, ylimmax-2, legend=c("Real Error", "Predicted Error"),
             col=c("black", "red"), lty=1:1, cex=0.8)
      # save to file
      test.result <- plotname
      write(test.result, 
            file="results/results_tests.txt",append=TRUE)
  
  } else if (test.type == "graphical"){
    # plot sops and error
    # plot to disk
    plotname= paste("./plots/","test_sop_",modelfilename,paste("RMSE",rmse.plot,sep="_"), sep="_" )
    jpeg(plotname, width = 768, height = 768, units = "px")
    par(mfrow=c(2,1))
    plot(
      xtest[,1],type="l", col="pink",
      main=paste("Test result",sep="",collapse=""),
      xlab="ms",
      ylab="SOP"
      #, xlim=c(450,500)
      )
    lines(xtest[,1021], col="yellow")
    lines(xtest[,2040], col="cyan")
    plot(
      ytest,type="l", 
      main=paste("Test result",sep="",collapse=""),
      xlab=paste(plotname,sep="",collapse=""),
      ylab="BER",
      ylim=c(ylimmin,ylimmax)
      #, xlim=c(450,500)
      )
    lines(ytest.pred, col="red")
    legend(1, ylimmax-2, legend=c("Real Error", "Predicted Error"),
           col=c("black", "red"), lty=1:1, cex=0.8)
    dev.off()
  }
  
}


test.model <- function(modelfilename, datafilename, subsel, xlab, test.type="numerical"){
  
  # modelfilename = "csv_small5_1000ms_100ms_400_40_10_250_rbf_sigma_1e-11_model.rda"
  # datafilename = "csv_small75_w100ms_d100ms_wp40_dp40_dsw10_maw250.Rdata"
  # subselect = 1500
  # xlab = "t in 10ms"
  
  # ,xtest, ytest
  load(file = paste("./data/preprocessed/",datafilename,sep="") ) # output is df3
  if (test.type == "numerical"){
    subselect <- sample(1:nrow(df3),subsel) # to correctly compute the error on a random sampling
    
  } else {
    # test.type=="graphical"
    # this test is for showing how the system predicts during an experiment
    # no sampling is done, and the plot must be rearranged to account for previous and further time windows
    start.point <- sample(1:(nrow(df3)-subsel),1)
    print(paste(" testing starts at ",start.point))
    subselect <- start.point:(start.point + subsel) # to correctly plot the prediction in an experiment
      
  }
  
  # if nrow(df3)<subselect -> pad with same data -> don't need to pad, it's only the columns that should match
  df <- df3[1,]
  if (nrow(df3) < subsel){
  for (i in 2:subsel){
      df <- rbind(df,df3[i,])
    }
  } else df <- df3[subselect,]
  
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  xtest <- df[,-remove.cols]
  ytest <- df[,grep("^futurey$", colnames(df))]
  
  # load model
  load(file = paste("./models/",modelfilename,sep="")) # output to "model" object
  
  # dim verification
  # extract dim from model name
  correct <- dimension.verification(modelfilename,xtest)
  if (correct!=TRUE) return(NULL)
  
  #predict
  if (class(model)[1] == "ksvm"){
    library(kernlab)
    ytest.pred <- predict(object = model, newdata = xtest, type="response")
  } else ytest.pred <- predict(model, xtest)
  # rmse
  rmse.test = sqrt(sum((ytest - ytest.pred)^2)/nrow(xtest))
  rmse.plot = round(rmse.test,3)
  
  # plot and save
  plot.and.save.test.result(xtest,ytest,ytest.pred,modelfilename,datafilename,rmse.plot, test.type)

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

model.functions <- list("rvm.rbf" = rvm.rbf, "svm.rbf" = svm.rbf)

