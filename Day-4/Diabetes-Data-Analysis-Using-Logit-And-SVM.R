# Set library to play with
library(RCurl)
library(corrplot)
library(e1071)
library(InformationValue)
library(stats)
library(randomForest)
library(caret)

# Take data from internet (UCI Datasets). 
# You need internet connection to grab the dataset
df = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

# Check datatype
str(df)

# Change Classification datatype to factor
df$Classification = as.factor(df$Classification)

# Check class bias
table(df$Classification)
# Based on the result it looks like data quite balanced, nothing to worry about class bias

# Normalize data
scaled_df = scale(df[,-10])
scaled_df = data.frame(scaled_df)
scaled_df$Classification = df$Classification

# Search importance variabel using random forest
rf_result = randomForest(Classification ~. , data = scaled_df)
importance(rf_result)
# Based on importance result, the best 3 feature to use for prediction are Glucose, Resistin, and Age

label = as.character(scaled_df[,10])

for (i in 1:length(label)) {
  if (label[i] == "1") {
    label[i] = 0
  } else {
    label[i] = 1
  }
}

scaled_df$Classification = as.numeric(label)

# Split data to training and test data
index = sample(x = 1:nrow(scaled_df), size = nrow(scaled_df)*0.65, replace = FALSE)
training_data = scaled_df[index, c(1,3,8,10)]
test_Data = scaled_df[-index, c(1,3,8,10)]
str(training_data)

# Using logistic regression
logitMod = glm(Classification ~., data = training_data, family = binomial(link = "logit"))
summary(logitMod)

# Find predicted value based on logitModel
predicted = predict(logitMod, test_Data, type = "response")

# Find Optimal Cutoff
optCutOff = optimalCutoff(test_Data$Classification, predicted)[1] 

# Find miss classification error
missLogit = misClassError(test_Data$Classification, predicted, threshold = optCutOff)
missLogit

# Accuracy
logictAcc = 1 - missLogit

# Using SVM
training_data$Classification = factor(training_data$Classification)
svmMod = svm(Classification ~ ., data = training_data)

# Using SVM model to predict new value
predicted = predict(svmMod, test_Data, type = "response")

# Confussion table
table(pred = predicted, actual =test_Data$Classification)

# Accuracy
svmAcc = mean(predicted == test_Data$Classification)

# Summary
data.frame(Method = c("Logistic Regression", "Support Vector Machine"), Accuracy = c(logictAcc, svmAcc))
