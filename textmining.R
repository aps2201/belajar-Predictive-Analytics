library(tm)

URL = "http://www.cs.cornell.edu/people/pabo/movie-review-data/review_polarity.tar.gz"
download.file(URL,destfile = "reviews.tar.gz")
untar("reviews.tar.gz")
setwd("txt_sentoken")

SourcePos = DirSource(file.path(".", "pos"), pattern="cv")
SourceNeg = DirSource(file.path(".", "neg"), pattern="cv")
pos = Corpus(SourcePos)
neg = Corpus(SourceNeg)

reviews = c(pos, neg)
reviews

preprocess = function(corpus, stopwrds = stopwords("english")){
  library(SnowballC)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus,
                  content_transformer(removeNumbers))
  corpus = tm_map(corpus, removeWords, stopwrds)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, stemDocument)
  corpus
}

processed = preprocess(reviews)

term_documentFreq = TermDocumentMatrix(processed)

asMatrix = t(as.matrix(term_documentFreq))
Frequencies = colSums(asMatrix)
head(Frequencies[order(Frequencies, decreasing=T)], 5)

Present = data.frame(asMatrix)
Present [Present>0] = 1

DocFrequencies = colSums(Present)
head(DocFrequencies[order(DocFrequencies, decreasing=T)], 5)

DocFrequencies[DocFrequencies > 1400]

total = ncol(asMatrix)
moreThanOnce = sum(DocFrequencies != Frequencies)
prop = moreThanOnce / total
moreThanOnce
total
prop

term_documentTfIdf= TermDocumentMatrix(processed,
                                       control = list(weighting = function(x) weightTfIdf(x,
                                                                                          normalize = TRUE)))

SparseRemoved = as.matrix(t(removeSparseTerms(
  term_documentTfIdf, sparse = 0.8)))

ncol(SparseRemoved)

sum(rowSums(as.matrix(SparseRemoved)) == 0)

colnames(SparseRemoved)

quality = c(rep(1,1000),rep(0,1000))

lengths = colSums(as.matrix(TermDocumentMatrix(processed)))

DF = as.data.frame(cbind(quality, lengths, SparseRemoved))

set.seed(123)
train = sample(1:2000,1000)
TrainDF = DF[train,]
TestDF = DF[-train,]
library(class) # knn() is in the class packages
library(caret) # confusionMatrix is in the caret package

set.seed(975)
Class3n = knn(TrainDF[,-1], TrainDF[,-1], TrainDF[,1], k = 3)
Class5n = knn(TrainDF[,-1], TrainDF[,-1], TrainDF[,1], k = 5)
confusionMatrix(Class3n,as.factor(TrainDF$quality))
