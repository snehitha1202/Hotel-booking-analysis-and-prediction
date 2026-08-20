library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)

setwd("~/Desktop/Hotel Booking Data ")

hotel <- read.csv("Output/hotel_bookings_cleaned.csv")

dim(hotel)

set.seed(123)

sample_index <- sample(
  1:nrow(hotel),
  size = 0.8 * nrow(hotel)
)

train_data <- hotel[sample_index, ]
test_data <- hotel[-sample_index, ]

train_data$is_canceled <- as.factor(train_data$is_canceled)
test_data$is_canceled <- as.factor(test_data$is_canceled)

dim(train_data)
dim(test_data)



logistic_model <- glm(
  is_canceled ~ lead_time +
    adr +
    hotel +
    market_segment +
    customer_type +
    deposit_type +
    previous_cancellations +
    booking_changes +
    total_of_special_requests,
  data = train_data,
  family = binomial
)

summary(logistic_model)



# Week 4 - Model Evaluation

# Predict cancellation on test data
predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Convert probability to predicted class
predicted_class <- ifelse(
  predicted_prob >= 0.5,
  1,
  0
)

predicted_class <- as.factor(predicted_class)

# Actual values
actual_class <- test_data$is_canceled

# Confusion Matrix
confusion_matrix <- confusionMatrix(
  predicted_class,
  actual_class,
  positive = "1"
)

confusion_matrix


# ROC Curve and AUC

roc_model <- roc(
  actual_class,
  predicted_prob
)

auc_value <- auc(roc_model)

auc_value

plot(
  roc_model,
  main = "ROC Curve - Logistic Regression",
  col = "blue",
  lwd = 2
)

abline(
  a = 0,
  b = 1,
  lty = 2
)
