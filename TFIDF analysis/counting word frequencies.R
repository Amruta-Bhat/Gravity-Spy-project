#import the comments file
file <- file.choose()

comments <- read.csv(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
View(comments)



#####################################################################
#Bi-gram analysis

sample_bigrams <- comments %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 2)

View(sample_bigrams)
str(sample_bigrams)

#stopwords = c("of","the","in ","like ","Inc."," Co\\.","LLC","Corporation","Corp","&")
#df$Company <- gsub(paste0(stopwords,collapse = "|"),"", df$Company)
getwd()
write.csv(sample_bigrams,"bigram_comments.csv")


#count the frequency of bigrams
gs_freq <- as.data.frame(table(sample_bigrams$bigram))
gs_freq <- gs_freq[order(-gs_freq$Freq), ]


#####################################################################
#Bi-gram analysis
library(dplyr)
library(tidytext)

sample_unigrams <- comments %>%
  unnest_tokens(bigram, comment_body, token = "ngrams", n = 1)

View(sample_unigrams)
str(sample_unigrams)

#stopwords = c("of","the","in ","like ","Inc."," Co\\.","LLC","Corporation","Corp","&")
#df$Company <- gsub(paste0(stopwords,collapse = "|"),"", df$Company)
getwd()
write.csv(sample_unigrams,"unigram_comments.csv")


#count the frequency of bigrams
gs_freq <- as.data.frame(table(sample_unigrams$bigram))
gs_freq <- gs_freq[order(-gs_freq$Freq), ]
