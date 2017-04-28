options(java.parameters = "- Xmx1024m")
library(NLP)
library(tm)
library(openNLP)
library(data.table)
library(koRpus)
library(XLConnectJars)
library(XLConnect)

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj5-grp14/data")

train <- read.csv("train.csv", header = T, as.is = T)
train$id <- train$id+1

S <- 50000
index <- 1:S
T1 <- train[index, ]

# decompose a string into words
dec <- function(z) {
  return(strsplit(z, split = " "))
}

# function tagPOS
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
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

# extract verb and noun
v_n <- function(input, v.n) {
  output <- sapply(strsplit(input$POStagged, paste("[[:punct:]]*/", v.n, ".?", sep = "")),
                   function(x) {res = sub("(^.*\\s)(\\w+$)", "\\2", x); res[!grepl("\\s", res)]} )
  return(output)
}

v_n.length <- function(input1, input2) {
  Q1TV <- v_n(input1, "VB")
  Q1TN <- v_n(input1, "NN")
  Q2TV <- v_n(input2, "VB")
  Q2TN <- v_n(input2, "NN")
  # original number of verb and noun
  LV1 <- length(Q1TV)
  LV2 <- length(Q2TV)
  LN1 <- length(Q1TN)
  LN2 <- length(Q2TN)
  Q1TV <- rem_eng(Q1TV)
  Q2TV <- rem_eng(Q2TV)
  Q1TN <- rem_eng(Q1TN)
  Q2TN <- rem_eng(Q2TN)
  # number of verb and noun after remove stopwords
  LV.rem1 <- length(Q1TV)
  LV.rem2 <- length(Q2TV)
  LN.rem1 <- length(Q1TN)
  LN.rem2 <- length(Q2TN)
  if (LV.rem1 > LV.rem2) {Q <- Q1TV; Q1TV <- Q2TV; Q2TV <- Q}
  if (LN.rem1 > LN.rem2) {Q <- Q1TN; Q1TN <- Q2TN; Q2TN <- Q}
  # percentage of same verb and noun between two questions
  len.V <- LV.rem1 + LV.rem2 - sum(Q1TV %in% Q2TV)
  same.V <- sum(Q1TV %in% Q2TV)/len.V
  len.N <- LN.rem1 + LN.rem2 - sum(Q1TN %in% Q2TN)
  same.N <- sum(Q1TN %in% Q2TN)/len.N
  output <- c(LV1, LV2, LN1, LN2, LV.rem1, LV.rem2, LN.rem1, LN.rem2, same.V, same.N)
  output[is.na(output) == T] <- 0
  return(output)
}


#
f <- function(i) {
  # input two questions
  Q1 <- T1$question1[i]
  Q2 <- T1$question2[i]
  # number of words in each question
  L1 <- length(unlist(strsplit(Q1, " ")))
  L2 <- length(unlist(strsplit(Q2, " ")))
  # ratio of length
  ratio <- min(c(L1, L2)) / max(c(L1, L2))
  # remove punctuation and stopwords
  dic1 <- rem_eng(Q1)
  dic2 <- rem_eng(Q2)
  L.rem1 <- length(dic1)
  L.rem2 <- length(dic2)
  # percentage of same words between two questions
  len <- L.rem1 + L.rem2 - sum(dic1 %in% dic2)
  same <- sum(dic1 %in% dic2)/len
  # extract verb and noun in each question
  input1 <- tagPOS(Q1)
  input2 <- tagPOS(Q2)
  Q1TV <- v_n(input1, "VB")
  Q1TN <- v_n(input1, "NN")
  Q2TV <- v_n(input2, "VB")
  Q2TN <- v_n(input2, "NN")
  # original number of verb and noun
  LV1 <- length(Q1TV)
  LV2 <- length(Q2TV)
  LN1 <- length(Q1TN)
  LN2 <- length(Q2TN)
  Q1TV <- rem_eng(Q1TV)
  Q2TV <- rem_eng(Q2TV)
  Q1TN <- rem_eng(Q1TN)
  Q2TN <- rem_eng(Q2TN)
  # number of verb and noun after remove stopwords
  LV.rem1 <- length(Q1TV)
  LV.rem2 <- length(Q2TV)
  LN.rem1 <- length(Q1TN)
  LN.rem2 <- length(Q2TN)
  # percentage of same verb and noun between two questions
  len.V <- LV.rem1 + LV.rem2 - sum(Q1TV %in% Q2TV)
  same.V <- sum(Q1TV %in% Q2TV)/len.V
  len.N <- LN.rem1 + LN.rem2 - sum(Q1TN %in% Q2TN)
  same.N <- sum(Q1TN %in% Q2TN)/len.N
  output <- c(LV1, LV2, LN1, LN2, LV.rem1, LV.rem2, LN.rem1, LN.rem2, same.V, same.N)
  output[is.na(output) == T] <- 0
  return(c(L1, L2, L.rem1, L.rem2, same, ratio, output))
}

# feature: "same"
feature <- matrix(NA, nrow = S, ncol = 16)
for(i in 1:S) {
  feature[i,] <- f(i)
  gc(reset = TRUE)
}

name1 <- c("len.1", "len.2", "rem.len.1", "rem.len.2", "num.same", "len.ratio", "len.v.1", "len.v.2", "len.n.1"
           , "len.n.2", "rem.len.v.1", "rem.len.v.2", "rem.len.n.1", "rem.len.n.2", "v.ratio", "n.ratio")
names(feature) <- name1
write.csv(feature, file = "feature.csv")
