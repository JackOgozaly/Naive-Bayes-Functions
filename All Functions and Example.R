#This project creates custom naive bayes functions and includes an exmaple of how to use them

#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#The packages the functions requires
library(tm) #Used for text cleaning
library(tidytext) #Used for text cleaning
library(caret) #Used for data balancing
library(e1071) #Used for naive bayes model
library(matrixStats) #Used to extract key values from prediction matrix


#Package version: 
# matrixStats_0.57.0 e1071_1.7-4        caret_6.0-86       ggplot2_3.3.3      lattice_0.20-41    tidytext_0.3.0     tm_0.7-8           NLP_0.2-1 

text_clean <- function(text_input, # should be a columm from a dataframe
                       dataframe #should be the dataframe you want the clean text to be attached to
){    
  #These lines clean the text input
  tidy_text <- text_input
  tidy_text <- removePunctuation(tidy_text)
  tidy_text <- tolower(tidy_text)
  tidy_text <- removeNumbers(tidy_text) #This line is optional
  tidy_text <- removeWords(tidy_text, stop_words$word)
  #These lines create a new dataframe, attaches the clean text, and exports
  tidy_text_df <- as.data.frame(dataframe)
  tidy_text_df$tidy_text <- tidy_text
  assign("tidytext_df", tidy_text_df, envir = globalenv())
}


balanced_data <- function(x, #Dataframe
                          y, #Labels to be balanced
                          train_percent = .75, #What percent of the data do you wish to train on? 75% by default 
                          upsample= T) #If true upsamples the data, if false downsamples. True by default. 
{
  set.seed(42)
  df <- as.data.frame(x)
  df$label <- as.factor(y)
  rows <- sample(nrow(df))
  df <- df[rows, ]
  rownames(df) <- NULL
  observations <- as.numeric(nrow(df))
  observations <- ceiling(observations)
  train_upper_limit <- train_percent * observations
  train_upper_limit <- ceiling(train_upper_limit)
  train_data <- df[1:train_upper_limit, ]
  test_data <- df[train_upper_limit:observations, ]
  if(upsample==T){
    new.df <- as.data.frame(upSample(train_data, train_data$label))
    number <- nrow(new.df)
    new.df <- new.df[,-ncol(new.df)]
    colnames(test_data) <- colnames(new.df)
    new.df <- rbind(new.df, test_data)
    assign("balanced_df", new.df, envir = globalenv())
    print("The row to stop training at is:")
    print(number)
  }
  else{
    new.df <- as.data.frame(downSample(train_data, train_data$label))
    number <- nrow(new.df)
    new.df <- new.df[,-ncol(new.df)]
    colnames(test_data) <- colnames(new.df)
    new.df <- rbind(new.df, test_data)
    assign("balanced_df", new.df, envir = globalenv())
    print("The row to stop training at is:")
    print(number)
  }
}

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



predict_input <- function(x) #Should be a vector of text 
{
  #Converts to a document term matrix
  function_corpus <- VCorpus(VectorSource(x))
  function_dtm <- DocumentTermMatrix(function_corpus, 
                                     control = list(tolower = TRUE,removeNumbers = TRUE,
                                                    stopwords = TRUE,
                                                    removePunctuatio = TRUE,
                                                    stemming = TRUE))
  #Creates bag of words matrix 
  function_freq_words <- findFreqTerms(function_dtm,1) #This three here specifies how many times a word has to occur to be counted
  function_dtm_freq <- function_dtm[,function_freq_words]
  convert_counts <- function(x){
    x <- ifelse(x>0,"Yes","No") 
  }
  function_test <- apply(function_dtm_freq,MARGIN = 2,convert_counts)
  assign("predict.input", function_test, envir=globalenv())
}

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


#The following example illustrates how to use these functions
#This dataset examining airline tweet sentiment can be found at: 
#https://www.kaggle.com/crowdflower/twitter-airline-sentiment
Airlines_Tweet_Dataset <- read.csv("Tweets.csv")
Airlines_New_Data <- Airlines_Tweet_Dataset[2001:2500,]
Airlines_Tweet_Dataset <- Airlines_Tweet_Dataset[1:2000,]


#Clean our text vector
text_clean(Airlines_Tweet_Dataset$text, Airlines_Tweet_Dataset)
#Feed our cleaned text into our function that will balance the data off of sentiment
balanced_data(tidytext_df, tidytext_df$airline_sentiment)
#Train the model up to the number the balanced data function told us to
naive_bayes(balanced_df$tidy_text, balanced_df$label, 2862)
#you can view the stats of your model by running this line
head(confusion_matrix_stats)

#Now we can take data our model has never seen and predict on it
text_clean(Airlines_New_Data$text, Airlines_New_Data)
predict_input(tidytext_df$tidy_text)

#Feeding our data this new data and collection predictions
prediction <- predict(naive_bayes_classifier, predict.input)
head(prediction)

#Or we can use the confidence interval function to create a dataframe with the predictions and conf. levels
confidence_interval(naive_bayes_classifier, tidytext_df$tidy_text)
head(confidence_interval.df)

#Now we can attach other info to our df like the actual label and run analyses off it 
confidence_interval.df$Actual_Label <- tidytext_df$airline_sentiment
confidence_interval.df$is.match <- ifelse(confidence_interval.df$Prediction == confidence_interval.df$Actual_Label, 1 , 0)
head(confidence_interval.df)

#And we can make some graphs to check if our confidence level predicts whether or not a prediction is right 
library(ggthemes)
g <- ggplot(confidence_interval.df, aes(confidence, is.match)) + 
  geom_point(size=1) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial), 
              color="#A100FF", size=2) + theme_economist_white()
plot(g)