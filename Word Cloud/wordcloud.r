# Load Libraries
library(tidyverse)   
library(quanteda)    
library(tm)          
library(wordcloud)   
library(RColorBrewer) 

# Load the dataset. 
bbc_data <- read.csv("C:/Users/aaron/Downloads/bbc_data.csv", stringsAsFactors = FALSE)

str(bbc_data)
colnames(bbc_data) <- c("text", "category")
#corpus
corpus <- Corpus(VectorSource(bbc_data$text))

#clean
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%  
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus_clean)
dtm_matrix <- as.matrix(dtm)
word_freq <- colSums(dtm_matrix)

# sort the words by frequency in descending order.
word_freq <- sort(word_freq, decreasing = TRUE)

# Here are the top 10 most frequent words:
print(head(word_freq, 10))

# Now, let's compute TF-IDF
dtm_tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(dtm_tfidf)
tfidf_freq <- colSums(tfidf_matrix)

# Sort the words by their TF-IDF scores in descending order.
tfidf_freq <- sort(tfidf_freq, decreasing = TRUE)

# Here are the top 10 words
print(head(tfidf_freq, 10))

#visualization wordcloud
set.seed(1234) 
wordcloud(names(word_freq), 
          freq = word_freq, 
          min.freq = 5,    
          max.words = 200, 
          colors = brewer.pal(8, "Dark2"), 
          scale = c(3, 0.5))


# Short description of what I did:
# The word cloud represents the most frequent words in the dataset, where the larger words are more frequent.
# This gives us a sense of the central topics or subjects of the text.
# The TF-IDF values help us identify words that are particularly important or different in particular documents,
# and which can be useful for further analysis.