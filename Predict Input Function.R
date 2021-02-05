#Predict Input Function
#Simple function that converts an input into a type of object the model can predict off of 


#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Package Needed
library(tm) #Used to create the bag of words matrix 

#Package versions
# tm_0.7-8  NLP_0.2-1


predict_input <- function(x) #Should be a column of text 
{
  #Converts to a document term matrix
  function_corpus <- VCorpus(VectorSource(x))
  function_dtm <- DocumentTermMatrix(function_corpus, 
                                     control = list(tolower = TRUE,removeNumbers = TRUE,
                                                    stopwords = TRUE,
                                                    removePunctuatio = TRUE,
                                                    stemming = TRUE))
  #Creates bag of words matrix 
  function_freq_words <- findFreqTerms(function_dtm,3) #This three here specifies how many times a word has to occur to be counted
  function_dtm_freq <- function_dtm[,function_freq_words]
  convert_counts <- function(x){
    x <- ifelse(x>0,"Yes","No") 
  }
  function_test <- apply(function_dtm_freq,MARGIN = 2,convert_counts)
  assign("predict.input", function_test, envir=globalenv())
}

