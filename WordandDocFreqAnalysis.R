WordandDocFreqAnalysis <- function() {
  library(dplyr)
  library(janeaustenr)
  library(tidytext)
  
  book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)
  
  #print(book_words)
  
  total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))
  
  #print(total_words)
  
  book_words <- left_join(book_words, total_words)
  
  #print(book_words)
  
  library(ggplot2)
  
  ggplot(book_words, aes(n/total, fill = book)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~book, ncol = 2, scales = "free_y")
  
  freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()
  
  #print(freq_by_rank)
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  rank_subset <- freq_by_rank %>% 
    filter(rank < 500,
           rank > 10)
  
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  #print(rank_subset)
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_abline(intercept = -0.62, slope = -1.1, 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  book_tf_idf <- book_words %>%
    bind_tf_idf(word, book, n)
  
  #print(book_tf_idf)
  
  book_tf_idf %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  library(forcats)
  
  book_tf_idf %>%
    group_by(book) %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
  
  library(gutenbergr)
  physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                                meta_fields = "author")
  
  #print(physics)
  
  physics_words <- physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE)
  
  #print(physics_words)
  
  plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan", 
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
  #print(plot_physics)
  
  plot_physics %>% 
    group_by(author) %>% 
    slice_max(tf_idf, n = 15) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(tf_idf, word, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = "tf-idf", y = NULL) +
    facet_wrap(~author, ncol = 2, scales = "free")
  
  library(stringr)
  
  physics %>% 
    filter(str_detect(text, "_k_")) %>% 
    select(text)
  
  physics %>% 
    filter(str_detect(text, "RC")) %>% 
    select(text)
  
  mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                 "fig", "file", "cg", "cb", "cm",
                                 "ab", "_k", "_k_", "_x"))
  
  #print(mystopwords)
  
  physics_words <- anti_join(physics_words, mystopwords, 
                             by = "word")
  
  #print(physics_words)
  
  plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    mutate(word = str_remove_all(word, "_")) %>%
    group_by(author) %>% 
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    mutate(word = fct_reorder(word, tf_idf)) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
  
  #print(plot_physics)
  
  ggplot(plot_physics, aes(tf_idf, word, fill = author)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~author, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
  
}
