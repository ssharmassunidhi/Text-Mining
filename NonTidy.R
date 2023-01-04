NonTidyFormat <- function() {
  library(tm)
  data("AssociatedPress", package = "topicmodels")
  
  #print(AssociatedPress)
  
  terms <- Terms(AssociatedPress)
  head(terms)
  
  library(dplyr)
  library(tidytext)
  
  ap_td <- tidy(AssociatedPress)
  #print(ap_td)
  
  ap_sentiments <- ap_td %>%
    inner_join(get_sentiments("bing"), by = c(term = "word"))
  
  #print(ap_sentiments)
  
  library(ggplot2)

  ap_sentiments %>%
    count(sentiment, term, wt = count) %>%
    ungroup() %>%
    filter(n >= 200) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(n, term, fill = sentiment)) +
    geom_col() +
    labs(x = "Contribution to sentiment", y = NULL)
  
  data("data_corpus_inaugural", package = "quanteda")
  inaug_dfm <- data_corpus_inaugural %>%
    quanteda::tokens() %>%
    quanteda::dfm(verbose = FALSE)
  #print(inaug_dfm)
  
  inaug_td <- tidy(inaug_dfm)
  #print(inaug_td)
  
  inaug_tf_idf <- inaug_td %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))
  
  #print(inaug_tf_idf)
  
  library(tidyr)
  
  year_term_counts <- inaug_td %>%
    extract(document, "year", "(\\d+)", convert = TRUE) %>%
    complete(year, term, fill = list(count = 0)) %>%
    group_by(year) %>%
    mutate(year_total = sum(count))
  
  year_term_counts %>%
    filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
    ggplot(aes(year, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ term, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "% frequency of word in inaugural address")
  
  ap_td %>%
    cast_dtm(document, term, count)
  
  ap_td %>%
    cast_dfm(document, term, count)
  
  library(Matrix)
  
  m <- ap_td %>%
    cast_sparse(document, term, count)
  
  class(m)
  dim(m)
  
  library(janeaustenr)
  
  austen_dtm <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word) %>%
    cast_dtm(book, word, n)
  
  austen_dtm
  
  data("acq")
  #print(acq)
  
  acq_td <- tidy(acq)
  #print(acq_td)
  
  acq_tokens <- acq_td %>%
    select(-places) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")
  
  
  acq_tokens %>%
    count(word, sort = TRUE)
  
  acq_tokens %>%
    count(id, word) %>%
    bind_tf_idf(word, id, n) %>%
    arrange(desc(tf_idf))
  
  library(tm.plugin.webmining)
  library(purrr)
  
  company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
               "Twitter", "IBM", "Yahoo", "Netflix")
  symbol  <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", 
               "TWTR", "IBM", "YHOO", "NFLX")
  
  download_articles <- function(symbol) {
    WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
  }
  
  stock_articles <- tibble(company = company,
                           symbol = symbol) %>%
    mutate(corpus = map(symbol, download_articles))
  
  #print(stock_articles)
  
  stock_tokens <- stock_articles %>%
    mutate(corpus = map(corpus, tidy)) %>%
    unnest(cols = (corpus)) %>%
    unnest_tokens(word, text) %>%
    select(company, datetimestamp, word, id, heading)
  
  #print(stock_tokens)
  
  library(stringr)
  
  stock_tf_idf <- stock_tokens %>%
    count(company, word) %>%
    filter(!str_detect(word, "\\d+")) %>%
    bind_tf_idf(word, company, n) %>%
    arrange(-tf_idf)
  
  stock_tokens %>%
    anti_join(stop_words, by = "word") %>%
    count(word, id, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(contribution = sum(n * value)) %>%
    slice_max(abs(contribution), n = 12) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(contribution, word)) +
    geom_col() +
    labs(x = "Frequency of word * AFINN value", y = NULL)
  
  stock_tokens %>%
    count(word) %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    group_by(sentiment) %>%
    slice_max(n, n = 5, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    facet_wrap(~ sentiment, scales = "free") +
    labs(x = "Frequency of this word in the recent financial articles", y = NULL)
  
  stock_sentiment_count <- stock_tokens %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    count(sentiment, company) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  
  #print(stock_sentiment_count)
  
  stock_sentiment_count %>%
    mutate(score = (positive - negative) / (positive + negative)) %>%
    mutate(company = reorder(company, score)) %>%
    ggplot(aes(score, company, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    labs(x = "Positivity score among 20 recent news articles", y = NULL)
  
  
  
  
  

}