library(xgboost)

#------------------------------------------
# Reading the training data and the labels.
#------------------------------------------

train <- read.csv("./clean_dataset/train.csv", stringsAsFactors = FALSE)
labels <- read.csv("./clean_dataset/target.csv", stringsAsFactors = FALSE)

#--------------------------------------------------
# Dropping the ID and selecting the label variable.
#--------------------------------------------------

train <- train[, 2:121]
target <- labels$x

train <- data.matrix(train)
rm(labels)

#--------------------------------------------------------------------
# This is a custom evaluation metric defined on the fly -- F_1 score.
#--------------------------------------------------------------------

f_one <- function(preds, dtrain){

  #--------------------------------------------------------------------
  # This function calculates the F_1 score for the test and train data.
  # Input 1.: Predictions.
  # Input 2.: Training matrix.
  # Output: The F_1 score.
  #--------------------------------------------------------------------

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

#------------------------------------------
# Hyperparamater vectors are defined here.
#------------------------------------------

depths <- 1:20
etas <- 30:1/100

#-------------------------------------------------------------------------
# The grid search is performed here with the custom early stopping metric.
#-------------------------------------------------------------------------

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
