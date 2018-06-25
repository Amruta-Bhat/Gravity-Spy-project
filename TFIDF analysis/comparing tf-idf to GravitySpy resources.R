devtools::install_github("wrathematics/ngram")
library(ngram)


#look at the about Us page of Gravity Spy
about_gs <- read.table("C:\\Users\\Amruta\\Desktop\\Research group\\about_gs.txt",
                       header = FALSE)

s <- as.data.frame(t(about_gs))
str <- concatenate(s$V1, collapse=" ")

#unigram count
u <- ngram(str, n=1)
unigram_count <- (get.phrasetable(u))[,c(1,2)]

#bigram count
b <- ngram(str, n=2)
bigram_count <- (get.phrasetable(b))[,c(1,2)]

sum(unigram_count$ngrams %in% gs_sci_terms$word)

sum(bigram_count$ngrams %in% gs_sci_bigrams$bigram)


#look at the faq_gs
faq_gs <- read.table("C:\\Users\\Amruta\\Desktop\\Research group\\Comments unwind\\CSCW paper\\faq_gs.txt",
                     sep = "" , header = FALSE,
                     na.strings ="", stringsAsFactors= FALSE)

s <- as.data.frame(t(faq_gs))
str <- concatenate(s$V1, collapse=" ")

#unigram count
u <- ngram(str, n=1)
unigram_count <- (get.phrasetable(u))[,c(1,2)]

#bigram count
b <- ngram(str, n=2)
bigram_count <- (get.phrasetable(b))[,c(1,2)]

sum(unigram_count$ngrams %in% gs_sci_terms$word)

sum(bigram_count$ngrams %in% gs_sci_bigrams$bigram)




#look at the intro_gs
intro_gs <- read.table("C:\\Users\\Amruta\\Desktop\\Research group\\Comments unwind\\CSCW paper\\intro_gs.txt",
                     sep = "" , header = FALSE,
                     na.strings ="", stringsAsFactors= FALSE)

s <- as.data.frame(t(intro_gs))
str <- concatenate(s$V1, collapse=" ")

#unigram count
u <- ngram(str, n=1)
unigram_count <- (get.phrasetable(u))[,c(1,2)]

#bigram count
b <- ngram(str, n=2)
bigram_count <- (get.phrasetable(b))[,c(1,2)]

sum(unigram_count$ngrams %in% gs_sci_terms$word)

sum(bigram_count$ngrams %in% gs_sci_bigrams$bigram)



