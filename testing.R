
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

extract.from.name <- function(datafile){
  
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
  dswp= dsw2/0.25
  mawp= maw2/0.25/dswp
  
  return(list(w2,d2,wp2,dp2,dsw2,maw2,dswp,mawp))
}

test.model <- function(modelfilename, datafilename, subsel,xlab, test.type="numerical",error_data_raw=FALSE,start.point=-1){
  
  # modelfilename = "csv_small5_1000ms_100ms_400_40_10_250_rbf_sigma_1e-11_model.rda"
  # datafilename = "csv_small75_w100ms_d100ms_wp40_dp40_dsw10_maw250.Rdata"
  # subselect = 1500
  # xlab = "t in 10ms"
  

  # ,xtest, ytest
  load(file = paste("./data/preprocessed/",datafilename,sep="") ) # output is 
  
  # translate subsel from ms to datapoints
  # -> get dsw from filename -> then translate to datapoints
  res <- extract.from.name(datafilename)
  dsw<-res[[5]]
  dp<-res[[4]]
  print(paste("subsel in ms",subsel))
  subsel <- subsel/dsw
  print(paste("subsel in datapints",subsel))
  
  if (test.type == "numerical"){
    subselect <- sample(1:nrow(df3),subsel) # to correctly compute the error on a random sampling
    
  } else {
    # test.type=="graphical"
    # this test is for showing how the system predicts during an experiment
    # no sampling is done, and the plot must be rearranged to account for previous and further time windows
    if (start.point==-1) start.point <- sample(1:(min(nrow(df3)-subsel,1)),1)
    else {
      #convert from ms to data points
      start.point = start.point/dsw
    }
    print(paste(" testing starts at ",start.point))
    subselect <- start.point:(start.point + subsel) # to correctly plot the prediction in an experiment
    
  }
  
  print("verifying ranges")
  print(paste(" subselect in datapoints: ",subselect[1],subselect[length(subselect)]))
  print(paste(" subselect in ms: ",subselect[1]*dsw,subselect[length(subselect)]*dsw))
  
  group.row.verification(df3)
  
  # if nrow(df3)<subselect -> pad with same data -> don't need to pad, it's only the columns that should match
  df <- df3[1,]
  if (nrow(df3) < subsel){
    print(paste(" correction for dimension",nrow(df3),subsel))
    for (i in 2:subsel){
      df <- rbind(df,df3[i,])
    }
  } else df <- df3[subselect,]
  
  
  print(paste("nrow(df)",nrow(df)))

  
  #print(colnames(df)[2004:ncol(df)])
  remove.cols <- c(1, grep("^group$", colnames(df)),grep("^y$", colnames(df)),grep("^futurey$", colnames(df)), grep("^ys_.*", colnames(df)))
  xtest <- df[,-remove.cols]
  #print(remove.cols)
  #print(colnames(xtest)[2006:length(colnames(xtest))])
  remove.cols <- c(1, grep("^y$", colnames(df)),grep("^futurey$", colnames(df)))
  xplot <- df[,-remove.cols]
  print(paste("nrow(xplot)",nrow(xplot)))
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
  rmse.plot = round(sqrt(sum((ytest - ytest.pred)^2)/nrow(xtest)),3)
  
  # plot and save
  if (test.type=="numerical") plot.and.save.test.result(xtest,ytest,ytest.pred,modelfilename,datafilename,rmse.plot)
  else sop.plot(xplot, ytest, ytest.pred,modelfilename, datafilename, rmse.plot, dsw,dp,error_data_raw)
}
