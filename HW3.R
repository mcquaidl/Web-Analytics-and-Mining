# Module 3 Code
#install.packages("pdftools")
#install.packages("tm")
#install.packages("SnowballC")
# install.packages("qdap")
#install.packages("tau")
#install.packages("arules")

#set your working directory; You need to modify it to point to your working directory
setwd("/users/larrymcquaid/Downloads/") 

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
# library(qdap) # Quantitative discourse analysis of transcripts.
library(dplyr) # Data preparation and pipes %>%.
library(ggplot2) # Plot word frequencies.
library(scales) # Common data analysis activities.
library(pdftools)
library(tau) #tau: Text Analysis Utilities
library(stringr) # Using stringr package
library(class)

getReaders()
getSources()

#Part a: creating corpus
# Determining where the system file is, as I have saved the 20Newsgroup folder in here
system.file("texts",package="tm")  # where are the tm example files stored

##Test Folders
# load first folder
Space.Path.Loc <- system.file("texts","20Newsgroups","20news-bydate-test","sci.space",package="tm")
Space.Files <- DirSource(Space.Path.Loc)
test.space.corpus <- Corpus(URISource(Space.Files$filelist[1:100]), readerControl=list(reader=readPlain))

#Load second folder
rec.autos.Loc <- system.file("texts","20Newsgroups","20news-bydate-test","rec.autos",package="tm")
rec.files <- DirSource(rec.autos.Loc)
test.rec.corpus <- Corpus(URISource(rec.files$filelist[1:100]), readerControl=list(reader=readPlain))

##Train Folders
# load first folder
Space.Path.Loc <- system.file("texts","20Newsgroups","20news-bydate-train","sci.space",package="tm")
Space.Files <- DirSource(Space.Path.Loc)
train.space.corpus <- Corpus(URISource(Space.Files$filelist[1:100]), readerControl=list(reader=readPlain))

#Load second folder
rec.autos.Loc <- system.file("texts","20Newsgroups","20news-bydate-train","rec.autos",package="tm")
rec.files <- DirSource(rec.autos.Loc)
train.rec.corpus <- Corpus(URISource(rec.files$filelist[1:100]), readerControl=list(reader=readPlain))

#Combine corpus
combined.corpus <- c(train.space.corpus, test.space.corpus, train.rec.corpus, test.rec.corpus)

#Test to inspect the combined corpora
inspect(combined.corpus[[1]])

#Part b: Preprocessing
#Put words in all lower case
combined.corpus.proc <- tm_map(combined.corpus, content_transformer(tolower))
#check to see if this worked correctly
inspect(combined.corpus.proc[[1]])

#remove the stop words
combined.corpus.proc <- tm_map(combined.corpus.proc, removeWords, stopwords("english"))
#check to see if this worked correctly
inspect(combined.corpus.proc[[1]])

#remove punctuation
combined.corpus.proc <- tm_map(combined.corpus.proc, removePunctuation)
#check to see if this worked correctly
inspect(combined.corpus.proc[[1]])

#I chose not to stem documents because I do not like how it makes the text appear, though have included the code to do it below
#combined.corpus.proc <- tm_map(combined.corpus.proc, stemDocument)
#check to see if this worked correctly
#inspect(combined.corpus.proc[[1]])


#Part c: create the document-term-matrix
#refine the dtm to only include words of at least 2 letters or longer
corp.dtm <- DocumentTermMatrix(combined.corpus.proc, control = list(
  wordLengths=c(2,20),  # words between 2 and 20 characters long
  bounds=list(global=c(5,Inf)),  # only include words in DTM if they happen in 5 or more documents
  weighting=weightBin  # binary DTM instead of default term frequency DTM
))

#coerce to matrix
corp.dtm <- as.matrix(corp.dtm)

#find  terms that occur 100 times
#findFreqTerms(corp.dtm, 100)

#Part d: split the dtm
train.doc <- as.matrix(corp.dtm[c(1:100, 201:300),]) #split out training documents
test.doc <- as.matrix(corp.dtm[c(101:200, 301:400),]) #split out test documents

#ensure the proper dimensions were kept
dim(test.doc)
dim(train.doc)
dim(corp.dtm)

#Part e: Make a correct tags vector
#because the train vector is created as 100 sci.space, and then 100 rec.autos we will do sci first repeated 100 times, and then rec repeated 100 times
tags <- factor(c(rep('Sci',100), rep('Rec', 100)))
tags #check

#Part f: classify text using the kNN function
prob.test<- knn(train.doc, test.doc, tags, k = 2, prob=TRUE) 

#Part e: display classification results
document.number <- 1:length(prob.test)  # which document number - 1 through however many documents we predicted (test data, not train data)
predicted.classification <- levels(prob.test)[prob.test]  # what is the classifier predicting for the ith test document?
probability <- attributes(prob.test)$prob  # what percentage of the k nearest neighbors are the same type as what we are predicting for the ith test document?
correct.classification <- prob.test==tags
# in this case, the correct classifications for the test data is known and is the same for the train data
# So we can compare the predictions (in variable 'b' and also in 'prob.test') with the correct Tags
#see the dataframe below
result <- data.frame(Document=document.number, Predict=predicted.classification, 'Percentage of K Neighbors Agree'=probability, Correct=correct.classification)
result

#Part h: percentage of documents the classifier predicted correct classification
sum(prob.test==tags)/length(tags)
