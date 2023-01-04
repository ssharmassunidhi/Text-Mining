TextAnalys <- function(filename,location) {
  
  #calling all the required libraries
  library(tm)
  library(wordcloud)
  library(stringr)
  library(ggplot2)
  
  
  #conn will set a connection between RStudio and the file to be ready to open
  conn1 <- file(filename,open="r")
  
  #this function will read the file line by line with the connection established 
  textcontent <-readLines(conn1)
  print(textcontent)
  
  
  #this function will interpret each element of the file as a document.
  textcontent <- VectorSource(textcontent)
  
  #this function will create a vector corpus of all the individual documents that are created and treated by R
  textcontent <- VCorpus(textcontent)
  
  #this function will construct a term-document matrix or a document-term matrix, which has all the frequencies of the individual words
  tdm <- TermDocumentMatrix(textcontent)
  
  print(tdm)
  
  #converting term document matrix to a simple matrix for simple plotting and classification
  tdm <- as.matrix(tdm)
  print(tdm)
  
  #this function will form row and column sums and means for objects for the sparse matrix created
  w <- rowSums(tdm)
  w <- subset(w, w>2)
  x <- names(w)
  
  #assigning a location for the subsequent barplot to be created for being saved
  #pass the location dynamically from the Bootstrapfile
  #give meaningful name to plot.
  outputBP <- paste0(location,"WordFreqPlot_",format(Sys.time(), "%d-%b-%Y_%H.%M"), ".png")
  png(file= outputBP, width=600, height=350)
  
  #creating a barplot by text analysis
  barplot(w, las=2, col="gold")
  dev.off()
  
  
  ww <- sort(rowSums(tdm), decreasing = TRUE)
  
  #assigning a location for the subsequent wordcloud to be created for being saved
  outputWC <- paste0(location,"WordFreqWordcloud_",format(Sys.time(), "%d-%b-%Y_%H.%M"), ".png")
  png(file=outputWC, width=12, height=8, units='in', res=300)
  set.seed(1337)
  
  #creating a wordcloud by text analysis
  wordcloud(words = names(ww),
            freq = w,
            max.words = 30,
            random.order = F,
            min.freq = 2,
            colors = brewer.pal(8, 'Dark2'),
            scale = c(5, 0.3),
            rot.per = 0.7)
  
  #creating a data frame of all the words and their frequencies
  w<- as.data.frame(w)
  w$names <- x
  colnames(w) <- c('freq', 'word')
  
  #saving data frame as a csv file
  output1 <- paste0(location,"stockWordFreq_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(w, output1, row.names = FALSE)
  
  outputBP
  outputWC
  output1
  
  
}