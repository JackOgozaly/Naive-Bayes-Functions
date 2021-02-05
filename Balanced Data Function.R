#Balanced Data Function 
#Simple function that shuffles all the rows in a dataframe, and then upsamples or downsamples. 

#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Packages needed for this function
library(caret)

#Package versions:
#caret_6.0-86    ggplot2_3.3.3   lattice_0.20-41

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

