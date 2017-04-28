# install.packages("randomForest")
library(data.table)
library(dplyr)
library(randomForest)

# read in features
feature <- read.csv("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/allfeatures.csv", fill=TRUE, header = TRUE, stringsAsFactors = FALSE)
feature <- abs(feature[, -1])
Data <- read.csv("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/data/train.csv")
label <- Data$is_duplicate
S <- 50000
label <- label[1:S]


# Random forest
RandomForestExploration = function() {
  features <- train.data
  label <- train.labels
  rf_result <- randomForest(x = features, y = as.factor(label), ntree = 1500)
  best_ntree <- which.min(rf_result$err.rate[,"OOB"])
  err_rate <- c(rf_result$err.rate[100, "OOB"], rf_result$err.rate[500, "OOB"]
               , rf_result$err.rate[1000, "OOB"], rf_result$err.rate[1500, "OOB"])
  names(err_rate) <- c("100", "500", "1000", "1500")
  image <- plot(x = c(1:1500), y = rf_result$err.rate[,"OOB"], main = "Validation Error for Random Forest", xlab = "Number of Trees", ylab = "Validation Error")
  return(image)
}

RandomForestExploration()

RandomForesTrain <- function(data, label) {
  ### Train with decision model
  rf_fit <- randomForest(x = data, y = as.factor(label), ntree = 1000, mty = sqrt(ncol(data)))
  return(rf_fit)
}

RandomForestTest <- function(fit_train, dat_test){
  pred <- predict(fit_train, newdata=dat_test)
  return(pred)
}

RandomForestCV <- function(K = 5){
  n <- length(S)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  for (i in 1:K){
    train.d <- feature[s != i,]
    train.l <- label[s != i]
    test.d <- feature[s == i,]
    test.l <- label[s == i]
    fit <- RandomForesTrain(train.d, train.l)
    pred <- RandomForestTest(fit, test.d)  
    cv.error[i] <- mean(pred != test.l)  
  }
  return(cv.error)
}

rf.cv.err <- RandomForestCV()
save(rf.cv.err, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/rf.cv.err.RData")


# separate into training dataset and testing dataset
train.index <- 1:(S*0.8)
train.data <- feature[train.index, ]
train.labels <- label[train.index]
test.data <- feature[-train.index, ]
test.label <- label[-train.index]
# train model
rf_model <- RandomForesTrain(train.data, train.labels)
rf_predict <- RandomForestTest(rf_model, test.data)
train_pred <- RandomForestTest(rf_model, train.data)
err_rf_train <- mean(train_pred != train.labels)
err_rf_test <- mean(rf_predict != test.label)



save(rf_model, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/rf_para.RData")
save(err_rf_test, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/err_rf_test.RData")
save(err_rf_train, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/err_rf_train.RData")
save(rf_predict, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/rf_pred.RData")


matching_matrix_rf <- matching_matrix(test.label, rf_predict)
save(matching_matrix_rf, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output/rf_matching.RData")
