library(tidyverse)
library(ggplot2)

setwd("~/Desktop/Hotel Booking Data ")
hotel <- read.csv("Output/hotel_bookings_cleaned.csv")

dim(hotel)


overall_cancellation_rate <- mean(hotel$is_canceled) * 100
overall_cancellation_rate

table(hotel$is_canceled)

prop.table(table(hotel$is_canceled)) * 100


#hotel type
cancellation_hotel <- aggregate(
  is_canceled ~ hotel,
  data = hotel,
  FUN = mean
)

cancellation_hotel$is_canceled <-
  cancellation_hotel$is_canceled * 100

cancellation_hotel


#market segment
cancellation_segment <- aggregate(
  is_canceled ~ market_segment,
  data = hotel,
  FUN = mean
)

cancellation_segment$is_canceled <-
  cancellation_segment$is_canceled * 100

cancellation_segment


#customer type
cancellation_customer <- aggregate(
  is_canceled ~ customer_type,
  data = hotel,
  FUN = mean
)

cancellation_customer$is_canceled <-
  cancellation_customer$is_canceled * 100

cancellation_customer


#deposit type
cancellation_deposit <- aggregate(
  is_canceled ~ deposit_type,
  data = hotel,
  FUN = mean
)

cancellation_deposit$is_canceled <-
  cancellation_deposit$is_canceled * 100

cancellation_deposit



lead_time_cancelled <-
  hotel$lead_time[hotel$is_canceled == 1]

lead_time_not_cancelled <-
  hotel$lead_time[hotel$is_canceled == 0]

mean(lead_time_cancelled)
mean(lead_time_not_cancelled)

lead_time_test <- t.test(
  lead_time_cancelled,
  lead_time_not_cancelled
)

lead_time_test



adr_cancelled <-
  hotel$adr[hotel$is_canceled == 1]

adr_not_cancelled <-
  hotel$adr[hotel$is_canceled == 0]

mean(adr_cancelled)
mean(adr_not_cancelled)

adr_test <- t.test(
  adr_cancelled,
  adr_not_cancelled
)

adr_test



cor_data <- hotel[, c(
  "is_canceled",
  "lead_time",
  "adr",
  "previous_cancellations",
  "booking_changes",
  "total_of_special_requests",
  "stays_in_weekend_nights",
  "stays_in_week_nights",
  "adults",
  "children"
)]

cor_matrix <- cor(
  cor_data,
  use = "complete.obs"
)

cor_matrix["is_canceled", ]




set.seed(123)

sample_index <- sample(
  1:nrow(hotel),
  size = 0.8 * nrow(hotel)
)

train_data <- hotel[sample_index, ]
test_data <- hotel[-sample_index, ]

dim(train_data)
dim(test_data)