Tweetidf <- function() {
  tweets <- read.csv(file.choose(), header=T)
  str(tweets)
  
  library(tm)
  library(tidytext)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  texttweet <- Corpus(DirSource('/Users/Sunidhi Sharma/OneDrive/Desktop/immig'))
  inspect(texttweet)
  
  texttweet <- tm_map(texttweet, tolower)
  inspect(texttweet)
  
  texttweet <- tm_map(texttweet, removePunctuation)
  texttweet <- tm_map(texttweet, removeNumbers)
  texttweet <- tm_map(texttweet, removeWords, stopwords('english'))
  removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
  texttweet <- tm_map(texttweet, content_transformer(removeURL))
  texttweet <- tm_map(texttweet, removeWords, c('aapl', 'apple'))
  texttweet <- tm_map(texttweet, gsub, 
                      pattern = 'stocks', 
                      replacement = 'stock')
  texttweet <- tm_map(texttweet, stripWhitespace)
  inspect(texttweet)
  
  
  tdm <- TermDocumentMatrix(texttweet)
  print(tdm)
  tdm <- as.matrix(tdm)
  print(tdm)
  
  w <- rowSums(tdm)
  w <- subset(w, w>=25)
  w <- sort(rowSums(tdm), decreasing = TRUE)
  w <- data.frame(names(w), w)
  colnames(w) <- c('word', 'freq')
  
  w1 <- as.tbl(w)
  print(w1)
  
 #tf-idf analysis
  freq_by_rank <- w1 %>% 
    mutate(rank = row_number(), 
           `term frequency` = freq) %>%
    ungroup()
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  ggsave("tf.png")
  
  rank_subset <- freq_by_rank %>% 
    filter(rank < 20,
           rank > 10)
  
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`)) + 
    geom_abline(intercept = 1.41, slope = -0.4, 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  ggsave("tf1.png")
  
  w1 <- w1 %>% 
    mutate(Sum = sum(w1$freq))
  
  book_tf_idf <- w1 %>%
    bind_tf_idf(freq, word, Sum)
  
  print(book_tf_idf)
  
  book_tf_idf %>%
    select(-Sum) %>%
    arrange(desc(tf_idf))
  
  library(forcats)
  
  book_tf_idf %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, word)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~word, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
    
  ggsave("tf3.png")
  
  
}
