# knn

install.packages("class")
library(class)

install.packages("gmodels")
library(gmodels)

feature <- read.csv("allfeatures.csv", fill=TRUE, header = TRUE, stringsAsFactors = FALSE)
feature <- feature[, -1]
feature <- as.data.frame(feature)
label <- read.csv("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/data/train.csv")
label <- label$is_duplicate
label <- label[1:50000]
feature <- cbind(label, feature)
names(feature)[1] <- "classified"

table(feature$classified)

S <- 50000
set.seed(426)
train.index <- sample(1:S, S*0.6)

#feature_train_0 <- feature[1:12500,]
#feature_train_1 <- feature[12501:25000,]
#feature_test_0 <- feature[25001:37500,]
#feature_test_1 <- feature[37501:50000,]

#feature_train <- rbind(feature_train_0, feature_train_1)
#feature_test <- rbind(feature_test_0, feature_test_1)

feature_train <- feature[train.index,]
feature_test <- feature[-train.index,]

feature_train_labels <- feature_train[, 1]
feature_test_labels <- feature_test[, 1]

feature_train <- feature_train[-1]
feature_test <- feature_test[-1]

feature_test_pred <- knn(train = feature_train, test = feature_test, cl = feature_train_labels, k=5)
CrossTable(x = feature_test_labels, y = feature_test_pred, prop.chisq = FALSE )

err_knn <- numeric(99)
for(k in 2:100) {
  feature_test_pred <- knn(train = feature_train, test = feature_test, cl = feature_train_labels, k=k)
  result <- table(feature_test_labels, feature_test_pred)
  err_knn[k-1] <- (result[1,1] + result[2,2])/20000
}

plot(2:100, err_knn, xlab = "K", ylab = "Test error")
save(err_knn, file = "KNN_err.RData")
