setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output")

# install.packages("randomForest")
library("randomForest")

# read in features
feature_same <- read.csv("feature.csv")
feature_same <- feature_same[,-1]
feature_Q1 <- read.csv("parsing.Q1.csv")
feature_Q1 <- feature_Q1[,-1]
feature_Q2 <- read.csv("parsing.Q2.csv")
feature_Q2 <- feature_Q2[,-1]
feature_pars <- feature_Q1 - feature_Q2

#
sort(colSums(feature_Q1))
colSums(feature_Q2)

# combind all features
fea <- cbind(feature_same, feature_pars)

# separate into training dataset and testing dataset
set.seed(426)
S <- 50000
train.index <- sample(1:S, 10000)
train.model <- fea[train.index, ]
train.label <- train$is_duplicate[train.index]


 # Random forest
#Function used to explore and validate various random forest parameters. From this, it was determined that no more than 500 trees are needed.
RandomForestExploration = function()
{
  #image_features = t(read.csv("../data/sift_features.csv"))
  features <- train.model
  #image_labels = unlist(read.csv("../data/labels.csv"))
  label <- train.label
  #image_rf = randomForest(x = image_features, y = as.factor(image_labels), ntree = 1500)
  rf_result <- randomForest(x = features, y = as.factor(label), ntree = 1000)
  best_ntree <- which.min(rf_result$err.rate[,"OOB"])
  err_rate <- c(rf_result$err.rate[100, "OOB"], rf_result$err.rate[500, "OOB"]
               , rf_result$err.rate[1000, "OOB"])#, rf_result$err.rate[1500, "OOB"])
  
  #Plotting the results of the OOB estimate
  names(err_rate) = c("100", "500", "1000")#, "1500")
  #jpeg(filename = "../figs/RandomForestErrorOriginalFeatures.jpg")
  plot(x = c(1:1000), y = rf_result$err.rate[,"OOB"], main = "Validation Error for Random Forest", xlab = "Number of Trees", ylab = "Validation Error")
  #dev.off()
}

trainRandomForest = function(feature_filename, labels_filename, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(feature_filename))
  image_labels = unlist(read.csv(labels_filename))
  image_rf = randomForest(x = image_features, y = as.factor(image_labels), ntree = 500)
  train_time = (proc.time() - t)[3]
  cat("Elapsed time for Training Random Forest with 500 trees is ", train_time, " seconds \n")
  err_rate = image_rf$err.rate[500, "OOB"]
  cat("Validation Error rate for Random Forest with 500 trees is", err_rate, "\n") #.2895 for full feature set
  filename = "../output/RFModifiedFeature.RData"
  if(full_feature == TRUE)
  {
    filename = "../output/RFFullFeature.RData"
  }
  save(image_rf, file = filename)
  return(image_rf)
}

testRandomForest = function(rf_object, features_filename, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(features_filename))
  rf_predict = as.vector(predict(rf_object, image_features))
  test_time = (proc.time() - t)[3]
  cat("Elapsed prediction time for  Random Forest with 500 trees is ", test_time, " seconds \n")
  filename = "../output/RFModifiedPredictions.csv"
  if(full_feature == TRUE)
  {
    filename = "../output/RFFullFeaturePredictions.csv"
  }
  write.csv(rf_predict, file = filename)
  return(rf_predict)
}


test.index <- 1:S[-train.index]
set.seed(426)
test.index <- sample(test.index, 10000)
test.model <- fea[test.index,]
rf_predict <- as.numeric(as.vector(predict(rf_result, test.model)))
err <- mean(rf_predict != train$is_duplicate[test.index])
