rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file = 'data/preprocessed/real_original_features_w10_d10.Rdata')
load(file = 'data/preprocessed/disc_original_features_w10_d10.Rdata')

features <- c("x1","x2","x3","sx1","sx2","sx3")

trainModel <- function(df, features, algorithm) {
  library(e1071)
  features <- c(features, "futurey","real.futurey")
  df <- df[features]
  test_size <- floor(0.75*nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = test_size)
  
  train <- df[train_ind,]
  test <- df[-train_ind,]
  model <- NULL
  
  if(algorithm == "naiveBayes") {
    model <- naiveBayes(futurey ~ ., data=train)
  } else if(algorithm == "svm") {
    model <- svm(futurey ~ ., data = train)
  } else {
    print("algorithm should be naiveBayes or svm")
    return(NULL)
  }
  
  pred <- predict(model, test)
  pred <- as.numeric(as.character(pred))
  
  real <- test$real.futurey
  mape <- sum(abs(real-pred)/pred)/length(pred)
  
  return(list(model=model, mape=mape))
}

nb_res <- trainModel(df_final_disc, features, "naiveBayes")
svm_res <- trainModel(df_final_real, features, "svm")
