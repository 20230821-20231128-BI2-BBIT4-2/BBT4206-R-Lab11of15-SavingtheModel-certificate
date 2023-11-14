# Load required libraries
library(randomForest)
library(caret)
library(dplyr)

# Load your dataset (replace 'your_data.csv' with your actual data file path)
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
cm <- confusionMatrix(predictions, test$Disease)

# Print the confusion matrix
print(cm$table)

# Save the trained random forest model
saveRDS(model, file = "./models/saved_random_forest_model.rds")
