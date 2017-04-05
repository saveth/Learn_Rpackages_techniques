# Learn to Do WordCloud in R
#
# References:
# https://www.r-bloggers.com/building-wordclouds-in-r/
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#

##############
# Building Wordcloud in R
##############
pkg <- c("tm","SnowballC","wordcloud")
install.packages(pkg)

library(tm)
library(SnowballC)
library(wordcloud)
library("RColorBrewer")

jeopQ <- read.csv('./Data/JEOPARDY_CSV.csv', stringsAsFactors = FALSE)

# Format the Jeoepard Questions
jeopCorpus <- Corpus(VectorSource(jeopQ$Question)) # create a corpus
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument) #transform it to plain text
jeopCorpus <- tm_map(jeopCorpus, removePunctuation) # remove punctuation
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english')) # remove stopwords

# transform words to their stem word
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

#plot wordcloud
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE) # didn't work
# Error Message:
# Error in simple_triplet_matrix(i, j, v, nrow = length(terms), ncol = length(corpus),   : 
# 'i, j' invalid 






#################
# Word cloud fundamentals 
#################
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

text <- readLines("./Data/ml.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)

#text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#text cleaning
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


#Build Term-Text Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Create Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Find term with freq of 4
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "freedom", corlimit = 0.3)
head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
