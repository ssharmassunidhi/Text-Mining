idfanalysis <- function(filename,location) {
  
  #calling all the required libraries
  library(tm)
  library(wordcloud)
  library(stringr)
  library(ggplot2)
  
  #this function will take the location and the filename of the cleaned file
  #filename3 <- paste(location,filename3,sep="")
  #print(filename)
  
  #conn will set a connection between RStudio and the file to be ready to open
  conn3 <- file(filename,open="r")
  
  #this function will read the file line by line with the connection established 
  textcontent2 <-readLines(conn3)
  print(textcontent2)
  
  #this function will interpret each element of the file as a document.
  textcontent2 <- VectorSource(textcontent2)
  
  #this function will create a vector corpus of all the individual documents that are created and treated by R
  textcontent2 <- VCorpus(textcontent2)
  
  #this function will construct a term-document matrix or a document-term matrix, which has all the frequencies of the individual words
  tdm2 <- TermDocumentMatrix(textcontent2)
  
  
  print(tdm2)
  
  #converting term document matrix to a simple matrix for simple plotting and classification
  tdm2 <- as.matrix(tdm2)
  print(tdm2)
  
  #this function will form row and column sums and means for objects for the sparse matrix created
  w3 <- rowSums(tdm2)
  
  #converting to a data frame 
  w3 <- data.frame(names(w3), w3)
  
  #assigning column names to the data frame
  colnames(w3) <- c('word', 'freq')
  
  #converting to a tibble 
  w4 <- tibble::as.tibble(w3)
  print(w4)
  
  #getting frequencies of the words in the documents by rank (seeing how often they occur)
  freq_by_rank <- w4 %>% 
    mutate(rank = row_number(), 
           `term frequency` = freq) %>%
    ungroup()
  
  #creating a plot for the same frequencies
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  #saving as a csv file
  output7 <- paste0(location,"stockafreqrank_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(freq_by_rank, output7, row.names = FALSE)

  #saving plot as a png file
  outputGGP7 <- paste0(location,"stockidf_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP7, width=15, height=10)

  #creating a subset of the ranks between any values we want (for eg. here, between 10 and 20)
  rank_subset <- freq_by_rank %>% 
    filter(rank < 20,
           rank > 10)
  
  #creating a log function
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  
  #creating a plot
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`)) + 
    geom_abline(intercept = 1.41, slope = -0.4, 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  #saving as a csv file
  output8 <- paste0(location,"stockfreqrank1_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(freq_by_rank, output8, row.names = FALSE)

  #saving plot as a png file
  outputGGP8 <- paste0(location,"stockidf1_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP8, width=15, height=10)

  #getting sums of all the frequencies
  w4 <- w4 %>% 
    mutate(Sum = sum(w4$freq))
  
  #getting term frequencies of all the words and creating columns
  stock_tf_idf <- w4 %>%
    bind_tf_idf(freq, word, Sum)
  
  print(stock_tf_idf)
  
  #arranging by sum in descending order
  stock_tf_idf %>%
    select(-Sum) %>%
    arrange(desc(tf_idf))
  
  
  #calling required library
  library(forcats)
  
  #slicing the plot of term frequencies by the mean and creating a 45 degree line on the plot
  stock_tf_idf %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, word)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~word, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
  
  #saving as csv file
  output9 <- paste0(location,"stocktfidf_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(stock_tf_idf, output9, row.names = FALSE)
  
  #saving as a png file
  outputGGP9 <- paste0(location,"stocktf_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP9, width=15, height=10)

  output7
  output8
  output9
  outputGGP7
  outputGGP8
  outputGGP9
  
}