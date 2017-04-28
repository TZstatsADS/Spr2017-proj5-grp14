# Ann

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/output")

#install.packages("MASS")
# install.packages("neuralnet")

library(MASS)
library(caTools)
library(neuralnet)

feature <- read.csv("allfeatures.csv", fill=TRUE, header = TRUE, stringsAsFactors = FALSE)
feature <- feature[, -1]
feature <- as.data.frame(feature)
label <- read.csv("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/data/train.csv")
label <- label$is_duplicate
label <- label[1:50000]

feature <- feature[, -c(44, 53, 54, 58)]

maxs <- apply(abs(feature), 2, max)
mins <- apply(abs(feature), 2, min)
scaled.data <- as.data.frame(scale(feature, center = mins, scale = (maxs - mins)))
print(head(scaled.data,2))
data = cbind(label, scaled.data)
names(data)[1] <- "classified"

set.seed(426)
split = sample.split(data$classified, SplitRatio = 0.75)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)[1:16]
f <- paste(feats,collapse=' + ')
f <- paste('classified ~',f)
f <- as.formula(f)

nn <- neuralnet(f, train[,1:17], hidden = c(2, 2, 2), linear.output = FALSE)

predicted.nn.values <- compute(nn,test[,2:108])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$classified,predicted.nn.values$net.result)



