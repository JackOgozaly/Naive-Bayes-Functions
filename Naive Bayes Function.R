#Naive Bayes Function
#Function takes a column of text, column of labels, and the row number to stop training at and trains a model and outputs results 

#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Packages needed for this function
library(caret) #Used for confusion matrices 
library(e1071) #Used for the actual naive bayes model
library(tm) #Used for making corpuses and document term matrices 

#Package versions: 
#tm_0.7-8        NLP_0.2-1       e1071_1.7-4     caret_6.0-86    ggplot2_3.3.3   lattice_0.20-41

naive_bayes <- function(x, #independent variable; should be a vector of text
                        y, #dependent variable, should be a vector of labels
                        train_rows, #What row num should the model train up to? 
                        plot = TRUE, #Prints a confusion matrix by default
                        print_stats = TRUE) #Do you want the model to print confusion matrix results? 
{ 
  #Convert text data to a corpus and then a document term matrix so that the model can be trained off it
  function_corpus <- VCorpus(VectorSource(x))
  function_dtm <- DocumentTermMatrix(function_corpus, 
                                     control = list(tolower = TRUE,removeNumbers = TRUE,
                                                    stopwords = TRUE,
                                                    removePunctuatio = TRUE,
                                                    stemming = TRUE))
  x_dataframe <- as.data.frame(x)#Converts column to df
  
  if (is.factor(y) != TRUE) {
    y <- as.factor(y) #Converts labels to a factor so that classification can happen
  } 
  y_dataframe <- as.data.frame(y) #Converts column of labels to df
  
  #Divides the data into train and test
  observations <- as.numeric(nrow(x_dataframe))
  function_dtm_train <- function_dtm[1:train_rows,]
  function_dtm_test <- function_dtm[train_rows:observations,]
  function_train_labels <- y_dataframe[1:train_rows,]
  function_test_labels <- y_dataframe[train_rows:observations,]
  #Counting freq of words and training the naive bayes model
  function_freq_words <- findFreqTerms(function_dtm_train,5)
  function_dtm_freq_train <- function_dtm_train[,function_freq_words]
  function_dtm_freq_test <- function_dtm_test[,function_freq_words]
  convert_counts <- function(x){
    x <- ifelse(x>0,"Yes","No") 
  }
  function_train <- apply(function_dtm_freq_train,MARGIN = 2,convert_counts)
  function_test <- apply(function_dtm_freq_test,MARGIN = 2,convert_counts)
  #Trains the naive Bayes model 
  function_classifier <- naiveBayes(function_train, as.factor(function_train_labels))
  #Testing the classifier on the test data
  function_test_pred <- predict(function_classifier, newdata=function_test)
  #Exporting the classifier 
  assign("naive_bayes_classifier", function_classifier, envir=globalenv())
  #Creating statistical measures to evaluate model
  cfm <- as.data.frame(table(function_test_pred, function_test_labels))
  print("Model Accuracy is:")
  print(1-mean(function_test_pred != function_test_labels))
  cfm_stats <- confusionMatrix(function_test_pred, function_test_labels,positive = "pos")
  #If the user selected it this prints the cofusion matrix stats
  if(print_stats == TRUE){
    assign("confusion_matrix_stats", cfm_stats, envir=globalenv())
  }
  #if the user wants a plot this code runs
  if(plot == TRUE){
    #Plots Predicted vs Actual labels from our test data 
    print(ggplot(data = cfm,
                 mapping = aes(x = function_test_pred,
                               y = function_test_labels)) +
            geom_tile(aes(fill = Freq)) +
            geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
            scale_fill_gradient(low = "blue",
                                high = "red",
                                trans = "log") +ylab("Actual Labels\n") + 
            scale_x_discrete(guide = guide_axis(n.dodge=3))+ 
            xlab("\nPredicted Labels"))
  }
}


