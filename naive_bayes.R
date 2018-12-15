rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file = 'data/preprocessed/original_features_w10_d10.Rdata')
df <- df3[,0:8]
rm(df3)

library(e1071)
?naiveBayes
head(df)

test_size <- floor(0.7*nrow(df))

# Let's sample indices 10 times so have an average accuracy.
# Each sampling will use different seeds
accuracy <- 0
for(k in 1:10) {
  train_ind <- sample(seq_len(nrow(df)), size = test_size)
  
  train <- df[train_ind,]
  test <- df[-train_ind,]
  
  nb <- naiveBayes(futurey ~ ., data = train)
  nb_pred <- predict(nb, test)
  
  pred_res <- table(nb_pred, test$futurey)
  
  accuracy <- accuracy + sum(diag(pred_res))/sum(pred_res)
}

average_accuracy <- accuracy/10

