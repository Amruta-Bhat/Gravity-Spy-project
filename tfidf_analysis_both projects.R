library(tm)
library(rjson)
library(plyr)

###################################################################################
#Import both files


#import the new comments file
setwd("C:\\Users\\Amruta\\Desktop\\Research group\\Comments unwind\\CSCW paper\\New comments file")

comments <- read.csv("gravity-spy-comments_2018-02-10.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE)
str(comments$comment_body)

#import the snapshot comments file
Lines <- readLines("c:\\Users\\amruta\\Downloads\\snapshotcomments.json") 
ss <- as.data.frame(t(sapply(Lines, fromJSON)))
#str(ss$body)
#head(ss$body)
snapshot_serengeti_comments <- as.character(ss$body)

snapshot_serengeti_comments <- iconv(snapshot_serengeti_comments, 'UTF-8', 'ASCII')
snapshot_serengeti_comments <- as.data.frame(snapshot_serengeti_comments)
colnames(snapshot_serengeti_comments) <- c("comments")



#####################################################################################
#remove the links and convert to lower case

#Gravity Spy corpus

#remove links
comments$comment_body <- gsub('http\\S+\\s*', "", comments$comment_body)
#convert to lower case
comments$comment_body <- tolower(comments$comment_body)


#Snapshot Serengeti Corpus

#remove links
snapshot_serengeti_comments$comments <- gsub('http\\S+\\s*', "", snapshot_serengeti_comments$comments)
#convert to lower case
snapshot_serengeti_comments$comments <- tolower(snapshot_serengeti_comments$comments)



######################################################################################
#Remove the stopwords

stopWords <- c(stopwords("en"))


#gravity spy database
comments$comment_body <- as.character(comments$comment_body)
'%nin%' <- Negate('%in%')
comments$comment_body <-lapply(comments$comment_body, function(x) {
  chk <- unlist(strsplit(x," "))
  p <- chk[chk %nin% stopWords]
  paste(p,collapse = " ")
})
comments$comment_body <- as.character(comments$comment_body)

#Snapshot Serengeti database
snapshot_serengeti_comments$comments <- as.character(snapshot_serengeti_comments$comments)
'%nin%' <- Negate('%in%')
snapshot_serengeti_comments$comments <-lapply(snapshot_serengeti_comments$comments, function(x) {
  chk <- unlist(strsplit(x," "))
  p <- chk[chk %nin% stopWords]
  paste(p,collapse = " ")
})
snapshot_serengeti_comments$comments <- as.character(snapshot_serengeti_comments$comments)



#########################################################################################
#create the files as required for the final corpus


#Gravity Spy database

#create the file as required for the final corpus
gs_file_1 <- as.data.frame(comments$comment_body)
gs_file_1$project <- "Gravity Spy"
gs_file_1 <- gs_file_1[,c(2,1)]
colnames(gs_file_1) <- c("project","comment")



#create the file as required for the final corpus
ss_file_1 <- as.data.frame(snapshot_serengeti_comments$comments)
ss_file_1$project <- "Snapshot Serengeti"
ss_file_1 <- ss_file_1[,c(2,1)]
colnames(ss_file_1) <- c("project","comment")


#Create the corpus required 
unigram_file <- rbind(gs_file_1,ss_file_1)
unique(unigram_file$project)



################################################################
#TFIDF analysis for each board of Gravity Spy

#consider both documents as one corpus

library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)


?austen_books()
unigram_file$comment <- as.character(unigram_file$comment)
unigram_file$project <- as.factor(unigram_file$project)

book_words <- unigram_file %>%
  unnest_tokens(word, comment) %>%
  count(project, word, sort = TRUE) %>%
  ungroup()
book_words

total_words <- book_words %>% 
  group_by(project) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

#term frequency distribution in both projects
ggplot(book_words, aes(n/total, fill = project)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~project, ncol = 2, scales = "free_y")

#conclusion: There are very long tails to the right for these novels 
#(those extremely common words!) that we have not shown in these plots. 
#These plots exhibit similar distributionsfor all the novels, 
#with many words that occur rarely and fewer words that occur frequently.

book_words <- book_words %>%
  bind_tf_idf(word, project, n)
book_words


#Let's look at terms with high tf-idf in
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

gs_sci_terms <- arrange(book_words, project, desc(tf_idf))


#visualizing the high tfidf words

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(project) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = project)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~project, ncol = 2, scales = "free") +
  coord_flip()



##########################################################################
#bi-gram analysis of the projects
sample_bigrams <- unigram_file %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2)

head(sample_bigrams)
tail(sample_bigrams)

sample_bigrams %>%
  count(bigram, sort = TRUE)

bigram_tf_idf <- sample_bigrams %>%
  count(project, bigram) %>%
  bind_tf_idf(bigram, project, n) %>%
  arrange(desc(tf_idf))

gs_sci_bigrams <- arrange(bigram_tf_idf, project, desc(tf_idf))
paste(as.character(unique(gs_sci_terms$word[101:150])), collapse = ",")


