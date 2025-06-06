#Lewis Acid–Base - R Files for Instructors and Researchers
#Brandon J. Yik & Jeffrey R. Raker, Copyright 2021

#This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
#To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#DOI 10.17605/OSF.IO/TNBEV

###################################################################################

### THIS SECTION IS FOR SETTING THE WORKING DIRECTORY ###

#set working directory
#you'll need to set a "working directory" - this is the folder/location where this R file and your data will be stored

setwd("set-working-directory-here")

###################################################################################

### THIS SECTION IS FOR INSTALLING NECESSARY PACKAGES ###

#install these packages - you will only need to do this once the very first time

install.packages("tm")
install.packages("qdap")
install.packages("caret")
install.packages("pROC")
install.packages("ngram")

###################################################################################

### THIS SECTION IS FOR LOADING NECESSARY PACKAGES ###

#if you have previously installed the packages above, you can skip to loading the libraries

#load libraries
library(tm)
library(qdap)
library(caret)
library(pROC)
library(ngram)

###################################################################################

### THIS SECTION IS FOR PREPROCESSING THE FULL SET ###

#load full data set
full.set <- read.csv('full-data.csv')

#functions to clean data
tryTolower <- function(testo, ifErrorReturnText=FALSE)
{#tryCatch error
  try_error = tryCatch(tolower(testo), error=function(e) e)
  #if not an error
  if (!inherits(try_error, "error"))
    testo = tolower(testo) else testo = NA
    testo}

#partition of response
full.data  <- full.set$response
#changes capitalization
full.data <- tryTolower(full.data)
#removes non-alphanumeric characters
full.data <- multigsub("[^[:alnum]]", " ", full.data, fixed = TRUE)
#removes special characters
full.data <- iconv(full.data, "UTF-8", "ASCII", sub = "")
#replaces punctuation with a space
full.data <- multigsub(c("/", "\\", "-", "—", "+", ".", ";", "‚", ",", "(", ")", "[", "]", "%", "&", "?", "<", ">", "{", "}", "^", "*"), 
                        c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "), 
                       full.data, fixed = TRUE)

#removes any remaining punctuation
full.data <- removePunctuation(full.data)
#adds leading & trailing spaces to strings
full.data <- paste("  ", full.data, "  ")

#removes common English stopwords
full.data <- removeWords(full.data,stopwords('en'))
#custom stopwords
custom.stopwords <- as.character(read.delim('custom-stopwords.txt', header = FALSE)$V1)
#removes custom stopwords
full.data <- removeWords(full.data,custom.stopwords)

#imports patterns/replacements
patt.repl <- read.csv('replacements.csv')
#adds leading & trailing spaces to patterns & replacements
pattern <- paste("", as.character(patt.repl$pattern), "")
replacements <- paste("", as.character(patt.repl$replacement), "")
#conducts patterns & replacements
full.data <- multigsub(pattern, replacements, full.data, fixed = TRUE)

#collapses multiple spaces into a single space
full.data <- stripWhitespace(full.data) 
#removes leading & trailing whitespaces
full.data <- trimws(full.data)

###################################################################################

### THIS SECTION IS FOR GENERATING A CSV FILE OF ORIGINAL & CLEANED DATA ###

#csv of original & cleaned data
full.set$clean.data <- full.data
write.csv(full.set, "data-comparison.csv", row.names = FALSE)

###################################################################################

### THIS SECTION IS FOR GENERATING N-GRAMS (FEATURES) ###

#unigrams
str <- concatenate(lapply(full.data, "[", 1))
unigram.full.data <- ngram(str, n = 1)
unigram.list <- get.phrasetable(unigram.full.data)
write.csv(unigram.list, "unigram.csv", row.names = FALSE)

#bigrams
str <- concatenate(lapply(full.data, "[", 1))
bigram.full.data <- ngram(str, n = 2)
bigram.list <- get.phrasetable(bigram.full.data)
write.csv(bigram.list, "bigram.csv", row.names = FALSE)

###################################################################################

### THIS SECTION IS FOR PREPROCESSING THE TRAINING SET ###

#load training data set
train.set <- read.csv('train-data.csv')

#functions to clean data
tryTolower <- function(testo, ifErrorReturnText=FALSE)
  {#tryCatch error
    try_error = tryCatch(tolower(testo), error=function(e) e)
    #if not an error
    if (!inherits(try_error, "error"))
      testo = tolower(testo) else testo = NA
      testo}

#partition of response
train.data <- train.set$response
#changes capitalization
train.data <- tryTolower(train.data)
#removes non-alphanumeric characters
train.data <- multigsub("[^[:alnum]]", " ", train.data, fixed = TRUE)
#removes special characters
train.data <- iconv(train.data, "UTF-8", "ASCII", sub = "")
#replaces punctuation with a space
train.data <- multigsub(c("/", "\\", "-", "—", "+", ".", ";", "‚", ",", "(", ")", "[", "]", "%", "&", "?", "<", ">", "{", "}", "^", "*"), 
                        c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "), 
                        train.data, fixed = TRUE)

#removes any remaining punctuation
train.data <- removePunctuation(train.data)
#adds leading & trailing spaces to strings
train.data <- paste("  ", train.data, "  ")

#removes common English stopwords
train.data <- removeWords(train.data,stopwords('en'))
#custom stopwords
train.stopwords <- as.character(read.delim('custom-stopwords.txt', header = FALSE)$V1)
#removes custom stopwords
train.data <- removeWords(train.data,custom.stopwords)

#imports patterns/replacements
patt.repl <- read.csv('replacements.csv')
#adds leading & trailing spaces to patterns & replacements
pattern <- paste("", as.character(patt.repl$pattern), "")
replacements <- paste("", as.character(patt.repl$replacement), "")
#conducts patterns & replacements
train.data <- multigsub(pattern, replacements, train.data, fixed = TRUE)

#collapses multiple spaces into a single space
train.data <- stripWhitespace(train.data) 
#removes leading & trailing whitespaces
train.data <- trimws(train.data)

###################################################################################

### THIS SECTION IS FOR PREPROCESSING THE VALIDATION SET ###

#load validation data set
valid.set <- read.csv('valid-data.csv')

#functions to clean data
tryTolower <- function(testo, ifErrorReturnText=FALSE)
{#tryCatch error
  try_error = tryCatch(tolower(testo), error=function(e) e)
  #if not an error
  if (!inherits(try_error, "error"))
    testo = tolower(testo) else testo = NA
    testo}

#initial clean data
valid.data <- valid.set$response
#changes capitalizations
valid.data <- tryTolower(valid.data)
#removes non-alphanumeric characters
valid.data <- multigsub("[^[:alnum]]", " ", valid.data, fixed = TRUE)
#removes special characters
valid.data <- iconv(valid.data, "UTF-8", "ASCII", sub = "")
#replaces punctuation with a space
valid.data <- multigsub(c("/", "\\", "-", "—", "+", ".", ";", "‚", ",", "(", ")", "[", "]", "%", "&", "?", "<", ">", "{", "}", "^", "*"), 
                                  c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "), 
                        valid.data, fixed = TRUE)

#removes any remaining punctuation
valid.data <- removePunctuation(valid.data)
#adds leading & trailing spaces to strings
valid.data <- paste("  ", valid.data, "  ")

#removes common English stopwords
valid.data <- removeWords(valid.data,stopwords('en'))
#custom stopwords
custom_stopwords <- as.character(read.delim('custom-stopwords.txt', header = FALSE)$V1)
#removes custom stopwords
valid.data <- removeWords(valid.data,custom_stopwords)

#imports patterns/replacements
patt_repl <- read.csv('replacements.csv')
#adds leading & trailing spaces to patterns & replacements
pattern <- paste("", as.character(patt_repl$pattern), "")
replacements <- paste("", as.character(patt_repl$replacement), "")
#conducts patterns & replacements
valid.data <- multigsub(pattern, replacements, valid.data, fixed = TRUE)

#collapses multiple spaces into a single space
valid.data <- stripWhitespace(valid.data) 
#removes leading & trailing whitespaces
valid.data <- trimws(valid.data)

###################################################################################

### THIS SECTION IS FOR GENERATING THE DOCUMENT-TERM MATRIX (DTM) FOR FULL SET ###

#match matrix function
match.matrix <- function(text.col, original.matrix = NULL, weighting = weightTf)
{control <- list(weighting = weighting) 
training.col <- sapply(as.vector(text.col, mode = "character"),iconv, to = "UTF8", sub = "byte")
corpus <- VCorpus(VectorSource(training.col))
matrix <- DocumentTermMatrix(corpus, control = control); if (!is.null(original.matrix)) 
{
  terms <- colnames(original.matrix[, which(!colnames(original.matrix) %in% colnames(matrix))]) 
  weight <- 0
  if (attr(original.matrix,"weighting")[2] == "tfidf") 
    weight <- 0.000000001
  amat <- matrix(weight,nrow=nrow(matrix), ncol=length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(matrix)
  fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% colnames(original.matrix))], amat), weighting = weighting)
  matrix <- fixed
}
matrix <- matrix[, sort(colnames(matrix))]
gc()
return(matrix)}

#creates document term matrix (DTM) of the full data
dtm.full.matrix <- match.matrix(full.data, original.matrix = NULL, weighting = tm::weightTf)
dtm.full.matrix

#set up DTM as a matrix
dtm.full <- as.data.frame(as.matrix(dtm.full.matrix))
#changes 1/0 as factors
dtm.full$correct_lewis <- as.factor(full.set$correct_lewis)
#processes 1/0 to yes/no
levels(dtm.full$correct_lewis) <- c("no", "yes")

###################################################################################

### THIS SECTION IS FOR MATCHING THE TRAINING SET DTM TO THE FULL SET DTM ###

#match matrix function
match.matrix <- function(text.col, original.matrix = NULL, weighting = weightTf)
{control <- list(weighting = weighting) 
training.col <- sapply(as.vector(text.col, mode = "character"),iconv, to = "UTF8", sub = "byte")
corpus <- VCorpus(VectorSource(training.col))
matrix <- DocumentTermMatrix(corpus, control = control); if (!is.null(original.matrix)) 
{
  terms <- colnames(original.matrix[, which(!colnames(original.matrix) %in% colnames(matrix))]) 
  weight <- 0
  if (attr(original.matrix,"weighting")[2] == "tfidf") 
    weight <- 0.000000001
  amat <- matrix(weight,nrow=nrow(matrix), ncol=length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(matrix)
  fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% colnames(original.matrix))], amat), weighting = weighting)
  matrix <- fixed
}
matrix <- matrix[, sort(colnames(matrix))]
gc()
return(matrix)}

#creates document term matrix (DTM) of training set
dtm.train.matrix <- match.matrix(train.data, original.matrix = dtm.full.matrix, weighting = tm::weightTf)
dtm.train.matrix

#set up DTM as a matrix
dtm.train <- as.data.frame(as.matrix(dtm.train.matrix))
#changes 1/0 as factors
dtm.train$correct_lewis <- as.factor(train.set$correct_lewis)
#processes 1/0 to yes/no
levels(dtm.train$correct_lewis) <- c("no", "yes")

###################################################################################

### THIS SECTION IS FOR MATCHING THE VALIDATION SET DTM TO THE FULL SET DTM ###

#creates document term matrix (DTM) of training set
dtm.valid.matrix <- match.matrix(valid.data, original.matrix = dtm.full.matrix, weighting = tm::weightTf)
dtm.valid.matrix

#set up DTM as a matrix
dtm.valid <- as.data.frame(as.matrix(dtm.valid.matrix))
#changes 1/0 as factors
dtm.valid$correct_lewis <- as.factor(valid.set$correct_lewis)
#processes 1/0 to yes/no
levels(dtm.valid$correct_lewis) <- c("no", "yes")

###################################################################################

### THIS SECTION IS FOR OTPIMIZATION OF PARAMETERS AND TRAINS THE MODEL ON TRAINING SET ###

#set seed - allows for a reproducible result
set.seed(1817)

#set up repeated k-fold cross-validation
train_control.2.2.tf <- trainControl(method = "repeatedcv", number = 2, repeats = 2, summaryFunction = twoClassSummary, classProbs = TRUE)

#sensitivity analysis & optimization of C parameter & train the model
set.seed(1817)
svm.parameters.grid <- expand.grid(C = c(0.0025, 0.0030, 0.0035, 0.0040, 0.0045, 0.0050, 0.0055, 0.0060, 0.0065, 0.0070, 0.0075))
svm.lewis <- train(correct_lewis ~ ., data = dtm.train, method = "svmLinear", preProc = c("center", "scale"), metric = "ROC", 
                   tuneGrid = svm.parameters.grid, trControl = train_control.2.2.tf)

###################################################################################

### THIS SECTION IS FOR EVALUATIION OF THE MODEL FOR FULL DATA ###

#predict for full data
set.seed(1817)
predict.full <- predict(svm.lewis, newdata = dtm.full)

#reprocesses yes/no to 1/0
full.set$predict.full <- predict.full
levels(full.set$predict.full) <- c("0", "1")

#creates confusion matrix
full.table <- table(predict.full, dtm.full$correct_lewis)

#gives AUC & evaluates confusion matrix
full.auc <- roc(predict.full, full.set$correct_lewis, levels = c("no", "yes"), direction = "auto")
full.conf <- confusionMatrix(full.table, positive = "yes", mode = "everything")

###################################################################################

### THIS SECTION IS FOR EVALUATIION OF THE MODEL FOR TRAINING DATA ###

#predict for full data
set.seed(1817)
predict.train <- predict(svm.lewis, newdata = dtm.train)

#reprocesses yes/no to 1/0
train.set$predict.train <- predict.train
levels(train.set$predict.train) <- c("0", "1")

#creates confusion matrix
train.table <- table(predict.train, dtm.train$correct_lewis)

#gives AUC & evaluates confusion matrix
train.auc <- roc(predict.train, train.set$correct_lewis, levels = c("no", "yes"), direction = "auto")
train.conf <- confusionMatrix(train.table, positive = "yes", mode = "everything")

###################################################################################

### THIS SECTION IS FOR EVALUATIION OF THE MODEL FOR VALIDATION DATA ###

#predict for full data
set.seed(1817)
predict.valid <- predict(svm.lewis, newdata = dtm.valid)

#reprocesses yes/no to 1/0
valid.set$predict.valid <- predict.valid
levels(valid.set$predict.valid) <- c("0", "1")

#creates confusion matrix
valid.table <- table(predict.valid, dtm.valid$correct_lewis)

#gives AUC & evaluates confusion matrix
valid.auc <- roc(predict.valid, valid.set$correct_lewis, levels = c("no", "yes"), direction = "auto")
valid.conf <- confusionMatrix(valid.table, positive = "yes", mode = "everything")

###################################################################################

### THIS SECTION IS FOR PARSING OUT FULL SET BY PROMPT TYPE & EVALUATES THE MODEL ###

#create subset tables by prompt type
full.pt <- filter(full.set, prompt == "pt")
full.lewis <- filter(full.set, prompt == "lewis")
full.acid <- filter(full.set, prompt == "acid")
full.base <- filter(full.set, prompt == "base")
full.ampho <- filter(full.set, prompt == "ampho")

#creates confusion matrix for prompt types
full.table.pt <- table(full.pt$predict.full, full.pt$correct_lewis)
full.table.lewis <- table(full.lewis$predict.full, full.lewis$correct_lewis)
full.table.acid <- table(full.acid$predict.full, full.acid$correct_lewis)
full.table.base <- table(full.base$predict.full, full.base$correct_lewis)
full.table.ampho <- table(full.ampho$predict.full, full.ampho$correct_lewis)

#gives AUC for prompt types
full.auc.pt <- roc(full.pt$predict.full, full.pt$correct_lewis)
full.auc.lewis <- roc(full.lewis$predict.full, full.lewis$correct_lewis)
full.auc.acid <- roc(full.acid$predict.full, full.acid$correct_lewis)
full.auc.base <- roc(full.base$predict.full, full.base$correct_lewis)
full.auc.ampho <- roc(full.ampho$predict.full, full.ampho$correct_lewis)

#evaluates confusion matrix
full.conf.pt <- confusionMatrix(full.table.pt, positive = "1", mode = "everything")
full.conf.lewis <- confusionMatrix(full.table.lewis, positive = "1", mode = "everything")
full.conf.acid <- confusionMatrix(full.table.acid, positive = "1", mode = "everything")
full.conf.base <- confusionMatrix(full.table.base, positive = "1", mode = "everything")
full.conf.ampho <- confusionMatrix(full.table.ampho, positive = "1", mode = "everything")

###################################################################################

### THIS SECTION IS FOR PARSING OUT TRAINING SET BY PROMPT TYPE & EVALUATES THE MODEL ###

#create subset tables by prompt type
train.pt <- filter(train.set, prompt == "pt")
train.lewis <- filter(train.set, prompt == "lewis")
train.acid <- filter(train.set, prompt == "acid")
train.base <- filter(train.set, prompt == "base")
train.ampho <- filter(train.set, prompt == "ampho")

#creates confusion matrix for prompt types
train.table.pt <- table(train.pt$predict.train, train.pt$correct_lewis)
train.table.lewis <- table(train.lewis$predict.train, train.lewis$correct_lewis)
train.table.acid <- table(train.acid$predict.train, train.acid$correct_lewis)
train.table.base <- table(train.base$predict.train, train.base$correct_lewis)
train.table.ampho <- table(train.ampho$predict.train, train.ampho$correct_lewis)

#gives AUC for prompt types
train.auc.pt <- roc(train.pt$predict.train, train.pt$correct_lewis)
train.auc.lewis <- roc(train.lewis$predict.train, train.lewis$correct_lewis)
train.auc.acid <- roc(train.acid$predict.train, train.acid$correct_lewis)
train.auc.base <- roc(train.base$predict.train, train.base$correct_lewis)
train.auc.ampho <- roc(train.ampho$predict.train, train.ampho$correct_lewis)

#evaluates confusion matrix
train.conf.pt <- confusionMatrix(train.table.pt, positive = "1", mode = "everything")
train.conf.lewis <- confusionMatrix(train.table.lewis, positive = "1", mode = "everything")
train.conf.acid <- confusionMatrix(train.table.acid, positive = "1", mode = "everything")
train.conf.base <- confusionMatrix(train.table.base, positive = "1", mode = "everything")
train.conf.ampho <- confusionMatrix(train.table.ampho, positive = "1", mode = "everything")

###################################################################################

### THIS SECTION IS FOR PARSING OUT VALIDATION SET BY PROMPT TYPE & EVALUATES THE MODEL ###

#create subset tables by prompt type
valid.pt <- filter(valid.set, promt == "pt")
valid.lewis <- filter(valid.set, prompt == "lewis")
valid.acid <- filter(valid.set, prompt == "acid")
valid.base <- filter(valid.set, prompt == "base")
valid.ampho <- filter(valid.set, prompt == "ampho")

#creates confusion matrix for prompt types
valid.table.pt <- table(valid.pt$predict.valid, valid.pt$correct_lewis)
valid.table.lewis <- table(valid.lewis$predict.valid, valid.lewis$correct_lewis)
valid.table.acid <- table(valid.acid$predict.valid, valid.acid$correct_lewis)
valid.table.base <- table(valid.base$predict.valid, valid.base$correct_lewis)
valid.table.ampho <- table(valid.ampho$predict.valid, valid.ampho$correct_lewis)

#gives AUC for prompt types
valid.auc.pt <- roc(valid.pt$predict.valid, valid.pt$correct_lewis)
valid.auc.lewis <- roc(valid.lewis$predict.valid, valid.lewis$correct_lewis)
valid.auc.acid <- roc(valid.acid$predict.valid, valid.acid$correct_lewis)
valid.auc.base <- roc(valid.base$predict.valid, valid.base$correct_lewis)
valid.auc.ampho <- roc(valid.ampho$predict.valid, valid.ampho$correct_lewis)

#evaluates confusion matrix
valid.conf.pt <- confusionMatrix(valid.table.pt, positive = "1", mode = "everything")
valid.conf.lewis <- confusionMatrix(valid.table.lewis, positive = "1", mode = "everything")
valid.conf.acid <- confusionMatrix(valid.table.acid, positive = "1", mode = "everything")
valid.conf.base <- confusionMatrix(valid.table.base, positive = "1", mode = "everything")
valid.conf.ampho <- confusionMatrix(valid.table.ampho, positive = "1", mode = "everything")

