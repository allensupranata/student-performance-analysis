# Read the file
setwd("C:/Users/User/OneDrive - Asia Pacific University/Documents/Asia Pacific University/Semester 3/PFDA")

df = read.csv("C:/Users/User/OneDrive - Asia Pacific University/Documents/Asia Pacific University/Semester 3/PFDA/PFDA Assignment-2309/student_prediction - Copy.csv", header = TRUE)
dataset = df[, c("SCHOLARSHIP", "WORK", "ATTEND", "NOTES", "GRADE")]
data = data.frame(dataset)
head(data)

# Load the necessary packages
library(xgboost)
library(caret)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- createDataPartition(data$GRADE, p = 0.7, list = FALSE)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# Define the X and Y variables
X_train <- subset(train_data, select = -GRADE) 
y_train <- train_data$GRADE
X_test <- subset(test_data, select = -GRADE)
y_test <- test_data$GRADE

# Convert data to matrix format for XGBoost --> data transformation
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Specify XGBoost parameters
params <- list(
  objective = "multi:softmax",  # for multi-class classification
  num_class = length(unique(data$GRADE)),  # number of classes
  eval_metric = "merror"  # log-likelihood loss for multi-class
)

# Train the XGBoost model
model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 50,  # number of boosting rounds
  verbose = 1  # set to 1 for more information during training
)

# Make predictions on the test set
predictions <- predict(model, newdata = as.matrix(X_test))
fy_test = as.factor(y_test)
all_levels <- levels(y_test)
predictions <- factor(predictions, levels = all_levels)

levels(predictions)
levels(y_test)

# Create confusion matrix
conf_matrix <- confusionMatrix(predictions, y_test)

# Print the confusion matrix
print(conf_matrix)

# Convert confusion matrix to a data frame
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

# Plot with ggplot2
ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("white", "forestgreen")) +
  theme_bw() +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted")

# Access feature importance values
importance_values <- xgb.importance(model = model)

# Print feature importance
print(importance_values)

# Plot feature importance
xgb.plot.importance(importance_matrix = importance_values)
