library(tm)
library(rjson)
library(plyr)
library(tidytext)
library(dplyr)


#Step 1: Run tfidf for GS, SS and Chimp and See corpus to get unigrams and bigrams


#Step 2: classify them into class and non-class terms


#Step 3: combine the corpus for unigrams and bigrams for both class and non-class terms


#Step 4: Store the corpus in a list called terms - used for Step 6: term frequency analysis
class_names_terms <- c('blip', 'whistle', 'noneoftheabove', 'powerline60hz', 'koifish', 'violinmodeharmonic', 'chirp', 'lowfrequencyburst', 'noglitch', 'scatteredlight', 'helix', 'lightmodulation', 'lowfrequencyline',  'paireddoves', 'aircompressor50hz', 'repeatingblips', 'scratchy',
           'tomte', 'wanderingline', 'extremelyloud')

non_class_terms <- c('hz',  'koi',  'possiblenewglitch',  'lfb',  'violin', '1080', 'harmonic', 'nota',  'modulation', '1400ripple', 'chirpish', 'ripple', '2000hz', 'ligo', 'gravitational',  'lacy',  'Hanford', 'morphology', 'doubleblip', 'midfrequencyline')

#Step 5: Import the file sent by Dhruv with class column 
file <- file.choose()

comments <- read.csv(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
View(comments)

#Step 6: Run the term frequency analysis code

#remove links
comments$comment_body <- gsub('http\\S+\\s*', "", comments$comment_body)
#convert to lower case
comments$comment_body <- tolower(comments$comment_body)

#remove stop words from the comments dataset
stopWords <- c(stopwords("en"))

comments$comment_body <- as.character(comments$comment_body)
'%nin%' <- Negate('%in%')
comments$comment_body <-lapply(comments$comment_body, function(x) {
  chk <- unlist(strsplit(x," "))
  p <- chk[chk %nin% stopWords]
  paste(p,collapse = " ")
})
comments$comment_body <- as.character(comments$comment_body)

#term frequencies based on user_type
View(comments)

user_level_0 <- comments[ which(comments$participation_level == "Class 0"),]
user_level_1 <- comments[ which(comments$participation_level == "Class 1"),]
user_level_2 <- comments[ which(comments$participation_level == "Class 2"),]
user_level_3 <- comments[ which(comments$participation_level == "Class 3"),]
user_level_4 <- comments[ which(comments$participation_level == "Class 4"),]


#term frequency counts
#Level 0 users
level_0_bigrams <- user_level_0 %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

level_0_freq <- as.data.frame(table(level_0_bigrams$bigram))
level_0_freq <- level_0_freq[order(-level_0_freq$Freq), ]
level_0_freq$Var1 <- as.character(level_0_freq$Var1)

#Level 1 users
level_1_bigrams <- user_level_1 %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

level_1_freq <- as.data.frame(table(level_1_bigrams$bigram))
level_1_freq <- level_1_freq[order(-level_1_freq$Freq), ]
level_1_freq$Var1 <- as.character(level_1_freq$Var1)

#Level 2 users
level_2_bigrams <- user_level_2 %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

level_2_freq <- as.data.frame(table(level_2_bigrams$bigram))
level_2_freq <- level_2_freq[order(-level_2_freq$Freq), ]
level_2_freq$Var1 <- as.character(level_2_freq$Var1)

#Level 3 users
level_3_bigrams <- user_level_3 %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

level_3_freq <- as.data.frame(table(level_3_bigrams$bigram))
level_3_freq <- level_3_freq[order(-level_3_freq$Freq), ]
level_3_freq$Var1 <- as.character(level_3_freq$Var1)


#Level 4 users
level_4_bigrams <- user_level_4 %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

level_4_freq <- as.data.frame(table(level_4_bigrams$bigram))
level_4_freq <- level_4_freq[order(-level_4_freq$Freq), ]
level_4_freq$Var1 <- as.character(level_4_freq$Var1)


#terms used - class terms
#level 0 users  
class_0_used_class_terms <- level_0_freq[(level_0_freq$Var1 %in% class_names_terms),]

#level 1 users  
class_1_used_class_terms <- level_1_freq[(level_1_freq$Var1 %in% class_names_terms),]

#level 2 users  
class_2_used_class_terms <- level_2_freq[(level_2_freq$Var1 %in% class_names_terms),]

#level 3 users  
class_3_used_class_terms <- level_3_freq[(level_3_freq$Var1 %in% class_names_terms),]

#level 4 users  
class_4_used_class_terms <- level_4_freq[(level_4_freq$Var1 %in% class_names_terms),]

length(unique(comments$comment_user_id))

#make file for stacked barplot
class_0_used_class_terms$type <- "Class 0"
class_1_used_class_terms$type <- "Class 1"
class_2_used_class_terms$type <- "Class 2"
class_3_used_class_terms$type <- "Class 3"
class_4_used_class_terms$type <- "Class 4"


class_0_used_class_terms$percentage <- NA
for(i in 1:nrow(class_0_used_class_terms)){
  class_0_used_class_terms$percentage[i] <- (class_0_used_class_terms$Freq[i]/sum(class_0_used_class_terms$Freq))*100
}

class_1_used_class_terms$percentage <- NA
for(i in 1:nrow(class_1_used_class_terms)){
  class_1_used_class_terms$percentage[i] <- (class_1_used_class_terms$Freq[i]/sum(class_1_used_class_terms$Freq))*100
}
class_2_used_class_terms$percentage <- NA
for(i in 1:nrow(class_2_used_class_terms)){
  class_2_used_class_terms$percentage[i] <- (class_2_used_class_terms$Freq[i]/sum(class_2_used_class_terms$Freq))*100
}
class_3_used_class_terms$percentage <- NA
for(i in 1:nrow(class_3_used_class_terms)){
  class_3_used_class_terms$percentage[i] <- (class_3_used_class_terms$Freq[i]/sum(class_3_used_class_terms$Freq))*100
}

class_4_used_class_terms$percentage <- NA
for(i in 1:nrow(class_4_used_class_terms)){
  class_4_used_class_terms$percentage[i] <- (class_4_used_class_terms$Freq[i]/sum(class_4_used_class_terms$Freq))*100
}


unigram_userType <- rbind(class_0_used_class_terms,class_1_used_class_terms, class_2_used_class_terms, class_3_used_class_terms, class_4_used_class_terms)
str(unigram_userType)
unigram_userType$type <- factor(unigram_userType$type)
unigram_userType$type <- factor(unigram_userType$type, levels = c("Class 0", "Class 1","Class 2", "Class 3", "Class 4"))


#facet graph
ggplot(data = unigram_userType, aes(x = type, y = percentage, fill = Var1)) + 
  geom_bar(stat = "identity", position="dodge") + 
  facet_wrap(~Var1, nrow = 4) 

#Step 7: make the table of the class and non-class terms 

#terms used - class terms
#level 0 users  
nc0 <- level_0_freq[(level_0_freq$Var1 %in% non_class_terms),]

#level 1 users  
nc1 <- level_1_freq[(level_1_freq$Var1 %in% non_class_terms),]

#level 2 users  
nc2 <- level_2_freq[(level_2_freq$Var1 %in% non_class_terms),]

#level 3 users  
nc3 <- level_3_freq[(level_3_freq$Var1 %in% non_class_terms),]

#level 4 users  
nc4 <- level_4_freq[(level_4_freq$Var1 %in% non_class_terms),]


#make file for stacked barplot
nc0$type <- "Class 0"
nc1$type <- "Class 1"
nc2$type <- "Class 2"
nc3$type <- "Class 3"
nc4$type <- "Class 4"


nc0$percentage <- NA
for(i in 1:nrow(nc0)){
  nc0$percentage[i] <- (nc0$Freq[i]/sum(nc0$Freq))*100
}

nc1$percentage <- NA
for(i in 1:nrow(nc1)){
  nc1$percentage[i] <- (nc1$Freq[i]/sum(nc1$Freq))*100
}
nc2$percentage <- NA
for(i in 1:nrow(nc2)){
  nc2$percentage[i] <- (nc2$Freq[i]/sum(nc2$Freq))*100
}
nc3$percentage <- NA
for(i in 1:nrow(nc3)){
  nc3$percentage[i] <- (nc3$Freq[i]/sum(nc3$Freq))*100
}

nc4$percentage <- NA
for(i in 1:nrow(nc4)){
  nc4$percentage[i] <- (nc4$Freq[i]/sum(nc4$Freq))*100
}


nc_userType <- rbind(nc0,nc1, nc2, nc3, nc4)
str(nc_userType)
nc_userType$type <- factor(nc_userType$type)
nc_userType$type <- factor(nc_userType$type, levels = c("Class 0", "Class 1","Class 2", "Class 3", "Class 4"))



#facet graph
ggplot(data = nc_userType, aes(x = type, y = percentage, fill = Var1)) + 
  geom_bar(stat = "identity", position="dodge") + 
  facet_wrap(~Var1, nrow = 4) 


ggplot(data = nc_userType, aes(x = Var1, y = percentage, fill = type)) + 
  geom_bar(stat = "identity", position="dodge") + coord_flip() + 
  facet_wrap(~type, nrow = 4) 

ggplot(data = unigram_userType, aes(x = Var1, y = percentage, fill = type)) + 
  geom_bar(stat = "identity", position="dodge") + coord_flip() +
  facet_wrap(~type, nrow = 4) 

write.csv(unigram_userType,"class_terms.csv")
write.csv(nc_userType,"non_class_terms.csv")


###normal bar graphs 
#library(ggplot2)
#ggplot(data = unigram_userType, aes(x = type, y = Freq, fill = Var1)) + 
#  geom_bar(stat = "identity",position="dodge") 

#ggplot(data = unigram_userType, aes(x = Var1, y = Freq, fill = type)) + 
#  geom_bar(stat = "identity", position="dodge") + coord_flip() + 
#  xlab("\nFrequency") +
#  ylab("Words\n") 


#exclude Class 4
#unigram_userType <- rbind(class_0_used_class_terms,class_1_used_class_terms, class_2_used_class_terms, class_3_used_class_terms)
#str(unigram_userType)
#unigram_userType$Var1 <- factor(unigram_userType$Var1)
#unigram_userType$type <- factor(unigram_userType$type)
#unigram_userType$type <- factor(unigram_userType$type, levels = c("Class 0", "Class 1","Class 2", "Class 3"))

#ggplot(data = unigram_userType, aes(x = type, y = Freq, fill = Var1)) + 
#  geom_bar(stat = "identity",position="dodge") 

#ggplot(data = unigram_userType, aes(x = Var1, y = Freq, fill = type)) + 
#  geom_bar(stat = "identity", position="dodge") + coord_flip() + 
#  xlab("\nFrequency") +
#  ylab("Words\n") + ggtitle("Class terms used by class 0 to 3 users") 

#reorder the graphs in descending order
#unigram_userType$Var1 <- factor(unigram_userType$Var1, levels = unigram_userType$Var1[order(unigram_userType$Freq)])
#str(unigram_userType)

#ggplot(data = class_4_used_class_terms, aes(x = Var1, y = Freq, fill = type)) + 
#  geom_bar(stat = "identity", position="dodge") + coord_flip() + 
#  xlab("\nFrequency") +
#  ylab("Words\n") + ggtitle("Class terms used by class 4 users")

#getwd()
#setwd("C:\\Users\\Amruta\\Desktop\\Research group\\Comments unwind\\CSCW paper\\New comments file\\DhruvFile")
#write.csv(unigram_userType, "unigram_userType.csv")




