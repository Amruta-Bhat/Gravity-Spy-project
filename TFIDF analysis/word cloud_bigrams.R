library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)



#import the comments file
file <- file.choose()

comments <- read.csv(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
View(comments)

#import the snapshot serengeti file
library(rjson)
Lines <- readLines("c:\\Users\\amruta\\Downloads\\snapshotcomments.json") 
business <- as.data.frame(t(sapply(Lines, fromJSON)))
str(business$body)
head(business$body)
snapshot_serengeti_comments <- as.character(business$body)

#Draw word cloud for the entire corpus of GS 
docs <- Corpus(VectorSource(comments$comment_body))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("can","get","now","already","like","http")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))


#plot bar graph for most popular words
wordcloud(docs, scale = c(3,0.75), max.words = 100, random.order = FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
?wordcloud

options(mc.cores=1)

#Draw bigram cloud for entire corpus of GS 

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)


#######################################################################################
##
##Snapshot Serengeti
##
#######################################################################################
#what are the hashtags on snapshot serengeti hashtags
#grep()

#Draw word cloud for Snapshot Serengeti
snapshot_serengeti_comments <- iconv(snapshot_serengeti_comments, 'UTF-8', 'ASCII')
docs <- Corpus(VectorSource(snapshot_serengeti_comments))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("can","get","now","already","like","http","#")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

#plot bar graph for most popular words
wordcloud(docs, scale = c(3,0.75), max.words = 100, random.order = FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
?wordcloud


#Draw bigram cloud for entire corpus of SS

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)


