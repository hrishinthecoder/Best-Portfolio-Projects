# Read File
apple <- read.csv(file.choose(), header=T)
str(apple)

# Build Corpus
library(tm)
corpus <- iconv(apple$text, to= "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean Text
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
clean_df <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
clean_df <- tm_map(clean_df, content_transformer(removeURL))
clean_df <- tm_map(clean_df, removeWords, c('aapl', 'apple'))
clean_df <- tm_map(clean_df, gsub, pattern = 'stocks', replacement = 'stock')
clean_df <- tm_map(clean_df, stripWhitespace)
inspect(clean_df[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(clean_df)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# World Cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.3)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           size = 0.7,
           shape = 'square',
           rotateRatio = 0.5,
           minSize = 1)


# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# read CSV
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to= 'utf-8')

# Obtain Sentiment Scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment("ugly")
get_nrc_sentiment("delay")

# barplot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "Sentiment Scores for Apple")