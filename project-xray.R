# Calling libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(textreadr)
library(wordcloud)
library(igraph)
library(ggraph)


#Importing all .txt files from one directory # a txt works like a csv file with multiple rows=
setwd("C:/Users/Anand/Desktop/compvis/project xray/txt")
nm <- list.files(path="C:/Users/Anand/Desktop/compvis/project xray/txt")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
my_txt_text <- as.data.frame(my_txt_text)
my_txt_text$V1 <- as.character(my_txt_text$V1)
#View(my_txt_text)


data("stop_words")
# Custom stopwords
custom <- data_frame(word = c("FINAL","REPORT","CHEST","chest","report","final","___"),
                     lexicon=rep("custom", each=7))

new_stopwords <- bind_rows(custom, stop_words)


# Tokenization
reports_tokenized <- my_txt_text %>%
  unnest_tokens(word, V1) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(reports_tokenized) # This is Tidy Format


# Bi-grams
my_bigrams <- my_txt_text %>%
  unnest_tokens(bigram, V1, token = "ngrams", n=2)

bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% new_stopwords$word) %>%
  filter(!word2 %in% new_stopwords$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE) 

bigram_united <- bigram_counts %>%
  unite(bigram, word1, word2, sep=" ")

print(bigram_united)


# WordClouds
reports_tokenized %>%
  with(wordcloud(word, n, max.words = 80))


# igraph & ggraph
bigram_graph <- bigram_counts %>%
  filter(n>35) %>%  # Lower this based on model say >1
  graph_from_data_frame()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

