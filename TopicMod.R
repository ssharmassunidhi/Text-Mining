TopicMod <- function(filename,location) {
  
  #calling all the required libraries
  library(tm)
  library(wordcloud)
  library(stringr)
  library(topicmodels)
  
  
  #this function will take the location and the filename of the cleaned file
  #filename4 <- paste(location4,filename4,sep="")
  #print(filename4)
  
  #conn will set a connection between RStudio and the file to be ready to open
  conn4 <- file(filename,open="r")
  
  #this function will read the file line by line with the connection established 
  textcontent3 <-readLines(conn4)
  print(textcontent3)
  
  #this function will interpret each element of the file as a document.
  textcontent3 <- VectorSource(textcontent3)
  
  #this function will create a vector corpus of all the individual documents that are created and treated by R
  textcontent3 <- VCorpus(textcontent3)
  
  #this function will construct a term-document matrix or a document-term matrix, which has all the frequencies of the individual words
  tdm3 <- TermDocumentMatrix(textcontent3)
  
  print(tdm3)
  
  #converting term document matrix to a simple matrix for simple plotting and classification
  tdm3 <- as.matrix(tdm3)
  print(tdm3)
  
  
  #this function will form row and column sums and means for objects for the sparse matrix created
  w5 <- rowSums(tdm3)
  
  #converting to a data frame 
  w5 <- data.frame(names(w5), w5)
  
  #assigning column names to the data frame
  colnames(w5) <- c('word', 'freq')
  
  
  #converting to a tibble 
  w5 <- tibble::as.tibble(w5)
  print(w5)
  
  #creating an LDA model using for example the VEM algorithm or Gibbs Sampling.
  tdm3 <- TermDocumentMatrix(Corpus(VectorSource(w5)))
  ap_lda <- LDA(tdm3, k = 2, control = list(seed = 1234))
  ap_topics <- tidy(ap_lda, matrix = "beta")
  
  #grouping by topics and slicing by the n
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 4) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  #creating a plot
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  
  #saving plot as a png file
  outputGGP10 <- paste0(location,"aptopterms_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP10, width=15, height=10)

  #saving as a csv file
  output10 <- paste0(location,"ap_top_terms_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(ap_top_terms, output10, row.names = FALSE)

  
  beta_wide <- ap_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    pivot_wider(names_from = topic, values_from = beta) %>% 
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))
  
  print(beta_wide)
  
  #saving the output
  output11 <- paste0(location,"ap_beta_wide_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(beta_wide, output11, row.names = FALSE)

  #Tidy a Corpus object from the tm package... Returns a data frame with one-row-per-document, with a text column containing the document's text, and one column for each local (per-document) metadata tag.
  ap_documents <- tidy(ap_lda, matrix = "gamma")
  print(ap_documents)
  
  #saving the output
  output12 <- paste0(location,"ap_documents_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(ap_documents, output12, row.names = FALSE)

  outputGGP10
  
  output10
  output11
  output12
  
  }