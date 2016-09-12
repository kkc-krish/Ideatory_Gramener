library(xgboost)

train <- read.csv("./clean_dataset/train.csv", stringsAsFactors = FALSE)
labels <- read.csv("./clean_dataset/target.csv", stringsAsFactors = FALSE)

train <- train[, 2:121]
target <- labels$x

train <- data.matrix(train)
rm(labels)

f_one <- function(preds, dtrain){

  #----------------
  #
  #
  #----------------

  preds <- preds
  labels <- as.integer(getinfo(dtrain, "label"))

  true_positive <- sum(rep(1, length(preds))[ (preds > 0.5) & (labels == 1) ])
  false_positive <- sum(rep(1, length(preds))[ (preds > 0.5) & (labels == 0) ])
  false_negative <- sum(rep(1, length(preds))[ (preds < 0.5) & (labels == 1) ])

  precision <- (true_positive/(true_positive + false_positive))
  recall <- (true_positive/(true_positive + false_negative))

  f_score <- (2*((precision * recall)/(precision + recall)))

  return(list(metric = "F-one", value = f_score))
}

depths <- 1:20
etas <- 30:1/100

for (d in 1:20){
  for (e in 1:30){

    bst <- xgb.cv(data = train,
                  label = target,
                  nfold = 5,
                  eta = e/100,
                  early.stop.round = 10,
                  max_depth = d,
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  min_child_weight = 1,
                  base_score = 0.95,
                  eval_metric = f_one,
                  nrounds = 5000,
                  objective = "binary:logistic",
                  maximize = TRUE)

    write.csv(bst, file = paste0("./grid_search_results/results_depth_", d, "_eta_", e/100, ".csv"), row.names = FALSE)
    
  }
}
