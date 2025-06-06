#Lewis Acid–Base - R Files for Instructors and Researchers
#Brandon J. Yik & Jeffrey R. Raker, Copyright 2021

#This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
#To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#DOI: 10.17605/OSF.IO/TNBEV

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
install.packages("plyr")
install.packages("caret")
install.packages("dplyr")
install.packages("gmodels")

###################################################################################

### THIS SECTION IS FOR LOADING NECESSARY PACKAGES ###

#if you have previously installed the packages above, you can skip to loading the libraries

#load libraries
invisible(library(tm))
invisible(library(qdap))
invisible(library(plyr))
invisible(library(caret))
invisible(library(dplyr))
invisible(library(gmodels))

###################################################################################

### THIS SECTION IS FOR PREPROCESSING THE FULL SET ###

cat("\n", file = 'demo-data.csv', append = TRUE)
data <- read.csv('demo-data.csv')

tryTolower <- function(testo, ifErrorReturnText=FALSE)
{try_error = tryCatch(tolower(testo), error=function(e) e)
  if (!inherits(try_error, "error"))
    testo = tolower(testo) else testo = NA
    testo}

mydata <- data$response
mydata <- tryTolower(mydata)
mydata <- multigsub("[^[:alnum]]", " ", mydata, fixed = TRUE)
mydata <- iconv(mydata, "UTF-8", "ASCII", sub = "")
mydata <- multigsub(c("/", "\\", "-", "—", "+", ".", ";", "‚", ",", "(", ")", "[", "]", "%", "&", "?", "<", ">", "{", "}", "^", "*"), 
                        c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "), 
                     mydata, fixed = TRUE)
mydata <- removePunctuation(mydata)
mydata <- paste("  ", mydata, "  ")
mydata <- removeWords(mydata,stopwords('en'))
custom.stopwords <- as.character(read.delim('custom-stopwords.txt', header = FALSE)$V1)
mydata <- removeWords(mydata,custom.stopwords)
patt.repl <- read.csv('replacements.csv')
pattern <- paste("", as.character(patt.repl$pattern), "")
replacements <- paste("", as.character(patt.repl$replacement), "")
mydata <- multigsub(pattern, replacements, mydata, fixed = TRUE)
mydata <- stripWhitespace(mydata) 
mydata <- trimws(mydata)

###################################################################################

### THIS SECTION IS FOR MATCHING THE YOUR DTM TO THE MODEL DTM ###

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

dtm.mydata.matrix <- match.matrix(mydata, original.matrix = dtm.full.matrix, weighting = tm::weightTf)
dtm.mydata <- as.data.frame(as.matrix(dtm.mydata.matrix))

###################################################################################

### THIS SECTION IS FOR EVALUATION OF THE MODEL FOR YOUR DATA ###

set.seed(1817)
predict <- predict(svm.lewis, newdata = dtm.mydata)
data$predict <- predict
data$predict.binary <- predict
levels(data$predict.binary) <- c("0", "1")
levels(data$predict)[levels(data$predict) == "no"] <- "incorrect/non-use"
levels(data$predict)[levels(data$predict) == "yes"] <- "correct"

###################################################################################

### THIS SECTION IS FOR MODEL SUMMARY OF YOUR DATA ###

CrossTable(data$predict)

###################################################################################

### THIS SECTION GENERATES A CSV FILE OF MODEL PREDICTIONS ###

write.csv(data, "demo-data-predicted.csv", row.names = FALSE)

