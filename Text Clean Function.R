#Very simple function that cleans the data in a standard way so that our model sees consistent data

#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Packages Needed
library(tm) #Used to remove certain character tpes
library(tidytext) #Used for stopwords dataset 

#Package Versions: 
# tm_0.7-8       NLP_0.2-1      tidytext_0.3.0

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
