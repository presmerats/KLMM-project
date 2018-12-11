
source("plot.R")



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
  #remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)), grep("^ys_.*", colnames(df)))
  
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
    print("incompatible parameters for dataset")
    return(FALSE)
  } else return(TRUE)
  
}

model.fitting <- function(model.func.string,params, X,Y,train.list, validation.list, test.list){
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
  
  
  return(list(model, dim(X[train.list[[1]],]), rmse.avg, total.time))
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
  dswp = dsw/0.25
  map = maw/0.25/dswp
  wp=round(w/0.25/dswp,0) # dsw = dswp*0.25 so w/dsw = w/0.25/dswp
  dp=round(d/0.25/dswp,0)
  
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
  fitting.results <- model.fitting(model.func.string,params, X,Y,train.list, validation.list, test.list)
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
  #predict.result <- test.model.internal(model, X[test.list[[1]],], Y[test.list[[1]]])
  #rmse <- predict.result["rmse"][[1]]
  #yvalidation.pred <- predict.result["pred"][[1]]
  #print(paste("test rmse",rmse))
  
}




cross_validate <- function(model.func.string, train.list, validation.list, X, Y) {
  modelfunc <- model.functions[model.func.string][[1]]
  
  sigmas = 10^seq(5,-5, by=-0.5)
  costs = 10^seq(0,5, by=0.5)
  epsilons = 10^seq(-6,0)
  k <- length(validation.list)
  
  # best model optimized with hyperparameters
  best_model <- NULL
  best_rmse <- NULL
  all_variables <- expand.grid(sigmas, costs, epsilons)
  cv_results <- matrix(nrow = nrow(all_variables), ncol = 2)
  colnames(cv_results) <- c("sigma_C_eps","RMSE")
  
  for(i in 1:nrow(all_variables)) {
    params = list(sigma=all_variables[i,1], C=all_variables[i,2],e=all_variables[i,3])
    print(paste(k,"-fold CV for: sigma=",params$sigma, " C=",params$C, " eps=", params$e, " ",(nrow(all_variables) - i)," left", sep=""))
    
    total_k_rmse <- 0
    for(j in 1:k) {
      xtrain <- as.matrix(X[train.list[[j]],])
      ytrain <- as.matrix(Y[train.list[[j]]])
      xvalidation <- as.matrix(X[validation.list[[j]],])
      yvalidation <- as.matrix(Y[validation.list[[j]]])
      
      model_cv <- modelfunc(xtrain, ytrain, params)
      if(!is.null(model_cv)) {
        # compute rmse
        res <- test.model.internal(model_cv, xvalidation, yvalidation, loss="rmse")
        rmse <- res[[2]]
        
        total_k_rmse <- total_k_rmse + rmse
      }
      
    }
    
    ave_k_rmse <- total_k_rmse/k
    
    if(is.null(best_rmse) || is.null(best_model)) {
      best_rmse <- ave_k_rmse
      best_model <- model_cv
    } 
    
    if(ave_k_rmse < best_rmse) {
      best_rmse <- ave_k_rmse
      best_model <- model_cv
    }
    
    key <- paste(params$sigma,params$C,params$e,sep="_")
    cv_results[i,1] <- key
    cv_results[i,2] <- ave_k_rmse
    print("KEY RMSE")
    print(cv_results[i,])
    
  }
  
  return(list(cv_results=cv_results, best_model=best_model))
}


train.model.alex <- function(datafile, model.func.string, params,k=3){
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
  dswp = params["dsp"][[1]]
  # translate from ms window to data points
  wp=round(w/0.25/dswp,0)
  dp=round(d/0.25/dswp,0)
  print(paste("wp",wp,"dp",dp))
  
  dswp = dsw/0.25
  map = maw/0.25/dswp
  wp=round(w/0.25/dswp,0)
  dp=round(d/0.25/dswp,0)
  
  
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
  validation.list <- subsets[[2]]
  test.list <- subsets[[3]]
  X <- subsets[[4]]
  Y <- subsets[[5]]
  
  #Cross-validate the model
  cv_results <- cross_validate(model.func.string, train.list, validation.list, X, Y)
  
  #Saving CV results
  save(cv_results, file = paste('results/cv/CV_',datafile,sep = ""))
  
  # train model with cross-validation
  print("training model")
  fitting.results <- model.fitting(model.func.string,params, X,Y,train.list, validation.list, test.list)
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
  #predict.result <- test.model.internal(model, X[test.list[[1]],], Y[test.list[[1]]])
  #rmse <- predict.result["rmse"][[1]]
  #yvalidation.pred <- predict.result["pred"][[1]]
  #print(paste("test rmse",rmse))
  
}

