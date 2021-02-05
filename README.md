# Naive-Bayes-Functions
Custom functions that easily make a naive bayes classifier and output unique results. 


Each function can be accessed seperaetely or all at once in the "All Functions..." file. The functions in this project are: 

text_clean: function that cleans text data

balanced_data: function that balances data with upsampling or downsampling. Also shuffles the rows. 

naive_bayes: function that trains and tests a model and outputs a confusion matrix plot and other statistics.

predict_input: converts an input into a bag of words matrix that the naive bayes model can predict off of.

confidence_interval: converts an input into a bag of words matrix that the naive bayes model can predict off of but also generates predictions off the input, grabs the model's confidence in the prediction, and generates the second most likley label. All this data is outputed in a dataframe. 

The confidence_interval and naive_bayes functions are the most impressive functions in this project and increase the functionality of standard approaches to naive bayes models in R. 
