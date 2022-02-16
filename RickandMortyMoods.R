install.packages("RWeka")
install.packages("ggsci")
system("sudo apt-get -y install libmagick++-dev", intern=TRUE)
devtools::install_github("hadley/devtools")
install.packages("igraph", type = "binary")
install.packages('ggraph')
install.packages("tm", repos="http://R-Forge.R-project.org")
require(devtools)
install_github("lchiffon/wordcloud2")
install.packages("tidytext")
install.packages("textdata")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("magick")
install.packages("circlize")
install.packages("fmsb")

library(readr)
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(textdata)
library(reshape2)
library(RWeka)
library(knitr)
library(gridExtra)
library(magick)
library(igraph)
library(ggraph)
library("ggsci")
library(devtools)
library(circlize)
library(radarchart)
install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr)
library(dplyr)
library(tm)
install.packages("tmap")
library(tmap)
install.packages("colorspace")
install.packages("ggplot2", dependencies = TRUE)
library("colorspace")
hcl_palettes(plot = TRUE)
library("ggplot2")
library(wordcloud2)
install.packages("viridis")
library("viridis")
install.packages("radarchart")
library(radarchart)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(reshape2)
library(grid)

scripts <- read.csv(file = "RickAndMortyScripts.csv")
bing <- read.csv(file = "Bing.csv")
nrc <- read.csv(file = "NRC.csv")
afinn <- read.csv(file = "Afinn.csv")

scripts = scripts %>% rename(index = "index",
                             season = "season",
                             episode = "episode",
                             episode.name = "episode.name",
                             name = "name",
                             line = "line")

head(scripts, 4)
tail(scripts, 4)
summary(scripts)


cleanCorpus <- function(text){
  #punctuation, whitespace, lowercase, numbers
  text.tmp <- tm_map(text, removePunctuation)
  text.tmp <- tm_map(text.tmp, stripWhitespace)
  text.tmp <- tm_map(text.tmp, content_transformer(tolower))
  text.tmp <- tm_map(text.tmp, removeNumbers)
  # removes stopwords
  stopwords_remove <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  text.tmp <- tm_map(text.tmp, removeWords, stopwords_remove)
  
  return(text.tmp)
}

frequentTerms <- function(text){
  
  #create matrix
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing = T)
  
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Bigram tokenizer
tokenizer_2 <- function(x){
  NGramTokenizer(x, Weka_control(min=2, max=2))
}

# Bigram function 
frequentBigrams <- function(text){
  
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer_2))
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=T)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Trigram tokenizer
tokenizer_3 <- function(x){
  NGramTokenizer(x, Weka_control(min=3, max=3))
}

# Trigram function 
frequentTrigrams <- function(text){
  
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer_3))
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=T)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Top 15 characters with the most dialogues
scripts %>% 
  # prepare the table
  count(name) %>%
  arrange(desc(n)) %>% 
  slice(1:15) %>%
  
  # the plot
  ggplot(aes(x=reorder(name, n), y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="#58D68D", high="#239B56") +
  labs(x="Character", y="Number of dialogues", title="Most talkative in Rick&Morty") +
  coord_flip() +
  theme_bw()

# categorizes words into +/-
tokens <- scripts %>%
  mutate(dialogue = as.character(scripts$line)) %>%
  unnest_tokens(word, line)

tokens %>% head(5) %>% select(name, word)

#wordcloud: +/- words

tokens %>% 
  # append the bing sentiment and prepare the data
  inner_join(bing, "word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=c("#6495ED", "#FF4500"), max.words = 100)

to_plot <- tokens %>% 
  # get 'bing' and filter the data
  inner_join(bing, "word") %>% 
  filter(name %in% c("Rick","Morty","Beth","Jerry","Summer")) %>% 
  
  # sum number of words per sentiment and character
  count(sentiment, name) %>% 
  group_by(name, sentiment) %>% 
  summarise(sentiment_sum = sum(n)) %>% 
  ungroup()

#NRC lexicon
sentiments <- tokens %>%
  inner_join(nrc, "word") %>%
  count(sentiment, sort=T)

sentiments


#Moods in Rick and Morty
# The plot:
sentiments %>% 
ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=F) +
  geom_label(label=sentiments$n) +
  labs(x="Sentiment", y="Frequency", title="Moods in Rick and Morty") +
  coord_flip() + 
  theme_bw()

#Smith family -- moods vs words

tokens %>% 
  inner_join(nrc, "word") %>% 
  count(sentiment, word, sort=T) %>% 
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  
  # Plot:
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  coord_flip() +
  theme_bw() +
  labs(x="Word", y="Frequency", title="Most Frequent Words vs Sentiments in Rick and Morty")


#Moods by family members
#Beth =  most trust, Summer = most anticipation, Morty = most fearful.
# Table with Character, sentiment and word count
char_sentiment <- tokens %>% 
  inner_join(nrc, "word") %>% 
  filter(name %in% c("Rick","Morty","Beth","Jerry","Summer")
         & !sentiment %in% c("positive","negative")) %>% 
  group_by(name, sentiment) %>% 
  count(name, sentiment) %>% 
  select(name, sentiment, char_sentiment_count=n)

# Total Count of sentiments per Character
total_char <- tokens %>% 
  inner_join(nrc, "word") %>% 
  filter(name %in% c("Rick","Morty","Beth","Jerry","Summer")
         & !sentiment %in% c("positive","negative")) %>% 
  count(name) %>% 
  select(name, total=n)

# Radar Chart:
char_sentiment %>% 
  inner_join(total_char, by="name") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(name, percent) %>% 
  chartJSRadar(showToolTipLabel = T, main="Character and Sentiment Radar", maxScale=22, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(c("#FA8072","#04700A","#062D82", "#7FFFD4" , "#F39C12")),
               lineAlpha = 0.7, polyAlpha = 0.05)

# Moods for Smith Family
tokens %>%
  #Data:
  filter(name %in% c("Rick", "Morty", "Beth", "Jerry", "Summer")) %>%
  inner_join(nrc, "word") %>%
  count(name, sentiment, sort=T) %>%
  
  #Plot:
  ggplot(aes(x=sentiment, y=n)) + 
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~name, scales="free_x") +
  labs(x="sentiment", y="frequency", title="Moods in Smith Family") +
  coord_flip() +
  theme_bw()


#AFINN lexicon ranking -5 to 5

tokens %>% 
  # Count how many word per value
  inner_join(afinn, "word") %>% 
  count(value, sort=T) %>%
  
  # Plot
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend = F, width = 0.5) +
  geom_label(aes(label=n)) +
  scale_fill_viridis(option="plasma") +
  scale_x_continuous(breaks=seq(-5, 5, 1)) +
  labs(x="Score", y="Frequency", title="Negative to Positive Range: Word Count Distribution") +
  theme_bw()

tokens %>% 
  # by word and value count number of occurences
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution = n * value,
         sentiment = ifelse(contribution<=0, "Negative", "Positive")) %>% #another variable
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  
  # plot
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=sentiment)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  labs(x="Word", y="Contribution", title="Most Impactful Words to Pos & Neg Moods") +
  coord_flip() +
  scale_fill_manual(values=c("33FF74", "#FFD700")) + 
  theme_bw()


# most frequent words
wordcloud2(frequentTerms(scripts$line), size=1.5, minSize = 0.9, color='random-light', backgroundColor="black", shape="pentagon", fontFamily="courier")

#Unigram: Most frequent words/Smith character
# Create a dataframe with stopwords
stopwords_script <- tibble(word = c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                                       "will","can","cant","dont","youve","us",
                                                       "youre","youll","theyre","whats","didnt")))
print(stopwords_script)


# Create the dataframe of tokens
scripts %>% 
  mutate(dialogue = as.character(scripts$line)) %>% 
  filter(name %in% c("Rick","Morty","Beth","Jerry","Summer")) %>% 
  
  # removes stopwords
  unnest_tokens(word, line) %>%
  anti_join(stopwords_script, by="word") %>%
  
  # top N frequent words per character
  count(name, word) %>% 
  group_by(name) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  
  mutate(word2 = factor(paste(word, name, sep="__"),
                        levels = rev(paste(word, name, sep="__")))) %>% 
  
  # the plot
  ggplot(aes(x=word2, y=n)) +
  geom_col(aes(fill = name), show.legend = F) +
  facet_wrap(~name, scales="free_y") +
  labs(x="Word", y="Frequency", title="10 Most Used Words for Smiths") +
  coord_flip() +
  theme_bw() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x))



