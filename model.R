# STEP 1. Install and Load the Required Packages ----
## arules ----
if (require("arules")) {
  require("arules")
} else {
  install.packages("arules", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## arulesViz ----
if (require("arulesViz")) {
  require("arulesViz")
} else {
  install.packages("arulesViz", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## tidyverse ----
if (require("tidyverse")) {
  require("tidyverse")
} else {
  install.packages("tidyverse", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readxl ----
if (require("readxl")) {
  require("readxl")
} else {
  install.packages("readxl", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## knitr ----
if (require("knitr")) {
  require("knitr")
} else {
  install.packages("knitr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## lubridate ----
if (require("lubridate")) {
  require("lubridate")
} else {
  install.packages("lubridate", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## plyr ----
if (require("plyr")) {
  require("plyr")
} else {
  install.packages("plyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naniar ----
if (require("naniar")) {
  require("naniar")
} else {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RColorBrewer ----
if (require("RColorBrewer")) {
  require("RColorBrewer")
} else {
  install.packages("RColorBrewer", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Load required libraries
library(caret)
library(randomForest)  # You can choose another classification algorithm

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("data/communicable_dataset.csv")

# STEP 2. Data Preprocessing ----
# Remove unnecessary columns
data <- data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")]

# Convert character data type to factor data type
data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")] <- lapply(data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")], as.factor)

# Replace missing values with the mode of the respective column
data <- data %>% 
    mutate_at(vars(Symptom_1:Symptom_8), ~ifelse(is.na(.), mode(., na.rm = TRUE), .))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Disease, p = .7, list = FALSE, times = 1)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]
# Load required libraries
library(caret)
library(randomForest)  # You can choose another classification algorithm

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("data/communicable_dataset.csv")

# STEP 2. Data Preprocessing ----
# Remove unnecessary columns
data <- data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")]

# Convert character data type to factor data type
data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")] <- lapply(data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")], as.factor)

# Replace missing values with the mode of the respective column
data <- data %>% 
    mutate_at(vars(Symptom_1:Symptom_8), ~ifelse(is.na(.), mode(., na.rm = TRUE), .))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Disease, p = .7, list = FALSE, times = 1)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Train a random forest model on the training data
model <- randomForest(Disease ~ ., data = train)

# Use the trained model to make predictions on the test data
predictions <- predict(model, test)

# Generate a confusion matrix
confusionMatrix(predictions, test$Disease)
# Train a random forest model on the training data
model <- randomForest(Disease ~ ., data = train)

# Use the trained model to make predictions on the test data
predictions <- predict(model, test)

# Generate a confusion matrix
cm <- confusionMatrix(predictions, test$Disease)
# Generate a confusion matrix
cm <- confusionMatrix(predictions, test$Disease)

# Print the confusion matrix
print(cm$table)

# Extract evaluation metrics
accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
f1_score <- cm$byClass["F1"]

# Print evaluation metrics
print(paste("Accuracy:", accuracy))

# Load required libraries
library(ggplot2)

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("data/communicable_dataset.csv")

# Load required libraries
library(ggplot2)

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("data/communicable_dataset.csv")

# Plot a histogram of the 'Disease' variable
ggplot(data, aes(x = Disease)) +
    geom_histogram(fill = "blue", alpha = 0.5) +
    labs(title = "Histogram of Diseases", x = "Disease", y = "Frequency")
    
# Load required libraries
library(arules)
library(arulesViz)
library(RColorBrewer)

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("data/communicable_dataset.csv")

# Convert character data type to factor data type
data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")] <- lapply(data[, c("Disease", "Symptom_1", "Symptom_2", "Symptom_3", "Symptom_4", "Symptom_5", "Symptom_6", "Symptom_7", "Symptom_8")], as.factor)

# Create a transaction object
trans <- as(data, "transactions")

# Create an item frequency plot for the top 10 items
itemFrequencyPlot(trans, topN = 10, type = "absolute",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Absolute Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(2, 2, 2, 2))

itemFrequencyPlot(trans, topN = 10, type = "relative",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Relative Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(2, 2, 2, 2))

# Train a random forest model on the training data
model <- randomForest(Disease ~ ., data = train)

# Use the trained model to make predictions on the test data
predictions <- predict(model, test)

# Calculate RMSE
rmse <- sqrt(mean((test$Disease - predictions)^2))

# Print RMSE
print(paste("RMSE:", rmse))

# Calculate SST
sst <- sum((test$Disease - mean(test$Disease))^2)

# Calculate SSR
ssr <- sum((predictions - mean(test$Disease))^2)

# Print SST and SSR
print(paste("SST:", sst))
print(paste("SSR:", ssr))
# Calculate MAE
mae <- mean(abs(test$Disease - predictions))

# Print MAE
print(paste("MAE:", mae))
