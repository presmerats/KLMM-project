rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file = 'data/preprocessed/original_features_w10_d10.Rdata')
df <- df3
rm(df3)

library(e1071)
head(df)

runNaiveBayes <- function(df, features) {
  features <- c(features, "futurey","real.futurey")
  df <- df[features]
  test_size <- floor(0.75*nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = test_size)
  
  train <- df[train_ind,]
  test <- df[-train_ind,]
  
  nb <- naiveBayes(futurey ~ ., data = train)
  pred <- predict(nb, test)
  pred <- as.numeric(as.character(pred))
  
  real <- test$real.futurey
  mape <- sum(abs(real-pred)/pred)/length(pred)
  
  return(list(model=nb, mape=mape))
}

features <- c("x1","x2","x3","sx1","sx2","sx3")

mod_res <- runNaiveBayes(df, features)

