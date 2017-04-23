library("NLP")
library("tm")

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/data")

train <- read.csv("train.csv", header = T, as.is = T)
train$id <- train$id+1
test <- read.csv("test.csv", header = T, as.is = T)

table(train$is_duplicate[1:10000])

set.seed(426)
index <- sample(1:nrow(train), 10000)
T1 <- train[index, ] # may need to change name

# decompose a string into words
dec <- function(z) {
  return(strsplit(z, split = " "))
}

# remove stopwords "english"
rem_eng <- function(input) {
  dic <- Corpus(VectorSource(input))
  dic <- tm_map(dic, content_transformer(tolower))
  dic <- tm_map(dic, removeWords, stopwords("english"))
  dic <- tm_map(dic, removePunctuation)
  dic <- lapply(dic, dec)
  dic <- unlist(dic)
  if ("" %in% dic) { dic <- dic[-which(dic == "")]}
  return(dic)
}

#
position <- function(i, dic1, dic2) {
  loc <- c()
  if (T1$is_duplicate[i] == 1) {
    1#...
  }
  
}

##
f <- function(i) {
  Q2 <- T1$question1[i]
  Q1 <- T1$question2[i]
  dic1 <- rem_eng(Q1)
  dic2 <- rem_eng(Q2)
  if (length(dic1) < length(dic2)) {Q <- dic1; dic1 <- dic2; dic2 <- Q}
  same <- sum(dic1 %in% dic2)/length(dic1)
  return(same)}

T1$same <- apply(as.matrix(c(1:10000)), 1, f)
T1$s2[T1$same < 0.5] <- 0.25
T1$s2[T1$same >= 0.5] <- 0.75
T1$s2 <- round(T1$same, 1)
table(T1$is_duplicate, T1$s2)

# T1 <- 1:10000

#   0.25 0.75
# 0 4015 2274
# 1  978 2733

#      0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9    1
# 0 1072  430 1029  842  633  683  333  435  526  182  124
# 1    2    1  103  344  508  716  488  540  589   63  357

# T1 <- random, set.seed(426)

#   0.25 0.75    1
# 0 4009 2282    1
# 1 1020 2688    0

#      0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9    1
# 0 1045  460 1019  799  671  637  347  464  514  211  124
# 1    2    1  113  398  475  694  487  511  606   67  354

#]freq <- sort(table(dic), decreasing = T) # frequency dictionary



