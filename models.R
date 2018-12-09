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

