options(java.parameters = "- Xmx1024m")
library(NLP)
library(tm)
library(openNLP)
library(data.table)
library(koRpus)
library(XLConnectJars)
library(XLConnect)

composition <- c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNS", "NNP"
                 , "NNPS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB"
                 , "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT","WP","WP$","WRB", ".", "-LRB-", "-RRB-", ","
                 , ":", "''", "``")

L <- length(composition)

# find word location in the total dictionary and compute the probability of each word
find_loc <- function(input) {
  input <- table(input$POStags)
  output <- rep(0, L)
  for (i in 1:length(input)) {
    index <- which(composition == names(input)[i])
    output[index] <- input[i]
  }
  return(output)
}

# composition of Q1, Q2
com <- function(input, Q) {
  # input two questions
  Ques <- ifelse(Q==1, input$question1, input$question2)
  # different composition in each question
  Questag <- tagPOS(Ques)
  output <- find_loc(Questag)
  return(output)
}

com.Q1 <- matrix(NA, nrow = S, ncol = L)
com.Q2 <- matrix(NA, nrow = S, ncol = L)
for(i in 44001: 45000) {
  com.Q1[i, ] <- com(T1[i, ], 1)
  com.Q2[i, ] <- com(T1[i, ], 2)
  gc(reset = TRUE)
}

names(com.Q1) <- composition
names(com.Q2) <- composition

write.csv(com.Q1, file = "parsing.Q1.csv")
write.csv(com.Q2, file = "parsing.Q2.csv")
