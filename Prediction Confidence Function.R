#Confidence Interval Function
#Function that converts an input to a bag of words matrix, runs the model on it, and collects the best guess, second best guess, and prob of first guess. 

#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Packages needed 
library(matrixStats)
library(tm)

#Package versions: 
#tm_0.7-8           NLP_0.2-1          matrixStats_0.57.0


confidence_interval <- function(x, #Should be a model
                                y) #Should be the model's input
{
  #Function that converts the input into a bag of words matrix
  predict_input <- function(x)
  {
    function_corpus <- VCorpus(VectorSource(x))
    function_dtm <- DocumentTermMatrix(function_corpus, 
                                       control = list(tolower = TRUE,removeNumbers = TRUE,
                                                      stopwords = TRUE,
                                                      removePunctuatio = TRUE,
                                                      stemming = TRUE))
    
    function_freq_words <- findFreqTerms(function_dtm,3)
    function_dtm_freq <- function_dtm[,function_freq_words]
    convert_counts <- function(x){
      x <- ifelse(x>0,"Yes","No") 
    }
    function_test <- apply(function_dtm_freq,MARGIN = 2,convert_counts)
    assign("predict.input", function_test, envir=globalenv())
  }
  y <- predict_input(y)
  #Runs prediction on on the text and generates probability of every label
  df <- as.matrix(predict(x, y, type="raw"))
  #Picks the highest value in the predicted dataframe
  max_row <- as.data.frame(rowMaxs(df, value = FALSE))
  #Change column name
  colnames(max_row) <- c("confidence")
  #Attach a column to our df with the best prediction from the model 
  max_row$Prediction <- predict(x,y)
  #Find and attach the column name for cell that has the second highest probability
  max_row$second_most_probable <- apply(df[,-ncol(df)], 1, 
                                        FUN = function(x) which(x == sort(x, decreasing = TRUE)[2]))
  max_row$second_most_probable <- colnames(df)[max_row$second_most_probable] 
  #Export to global enviornment so that it can be viewed
  assign("confidence_interval.df", max_row, envir=globalenv())
}

