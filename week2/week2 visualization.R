library(tidyverse)
library(ggplot2)
library(corrplot)

hotel <- read.csv("Output/hotel_bookings_cleaned.csv")

dim(hotel)

cancellation_by_hotel <- aggregate(
  is_canceled ~ hotel,
  data = hotel,
  FUN = mean
)

cancellation_by_hotel$is_canceled <-
  cancellation_by_hotel$is_canceled * 100

#visualization 1

barplot(
  cancellation_by_hotel$is_canceled,
  names.arg = cancellation_by_hotel$hotel,
  main = "Cancellation Rate by Hotel Type",
  xlab = "Hotel Type",
  ylab = "Cancellation Rate (%)"
)

#visualization 2

booking_by_hotel <- table(hotel$hotel)

barplot(
  booking_by_hotel,
  main = "Booking Distribution by Hotel Type",
  xlab = "Hotel Type",
  ylab = "Number of Bookings",
  col = "skyblue"
)

#visualizatioon 3

adr_by_hotel <- aggregate(
  adr ~ hotel,
  data = hotel,
  FUN = mean
)

barplot(
  adr_by_hotel$adr,
  names.arg = adr_by_hotel$hotel,
  main = "Average Daily Rate by Hotel Type",
  xlab = "Hotel Type",
  ylab = "Average ADR",
  col = "skyblue"
)

#visualization 4

lead_time_by_hotel <- aggregate(
  lead_time ~ hotel,
  data = hotel,
  FUN = mean
)

barplot(
  lead_time_by_hotel$lead_time,
  names.arg = lead_time_by_hotel$hotel,
  main = "Average Lead Time by Hotel Type",
  xlab = "Hotel Type",
  ylab = "Average Lead Time (Days)",
  col = "skyblue"
)

#visualization 5

bookings_by_segment <- table(hotel$market_segment)

barplot(
  bookings_by_segment,
  main = "Bookings by Market Segment",
  xlab = "Market Segment",
  ylab = "Number of Bookings",
  las = 2,
  col = "skyblue"
)

#visualization 6

customer_distribution <- table(hotel$customer_type)

barplot(
  customer_distribution,
  main = "Bookings by Customer Type",
  xlab = "Customer Type",
  ylab = "Number of Bookings",
  col = "skyblue"
)

#visualization 7

cancellation_by_segment <- aggregate(
  is_canceled ~ market_segment,
  data = hotel,
  FUN = mean
)

cancellation_by_segment$is_canceled <-
  cancellation_by_segment$is_canceled * 100

barplot(
  cancellation_by_segment$is_canceled,
  names.arg = cancellation_by_segment$market_segment,
  main = "Cancellation Rate by Market Segment",
  xlab = "Market Segment",
  ylab = "Cancellation Rate (%)",
  las = 2,
  col = "skyblue"
)


#visualization 8

monthly_bookings <- table(
  hotel$arrival_date_year,
  hotel$arrival_date_month
)

barplot(
  monthly_bookings,
  beside = TRUE,
  main = "Monthly Booking Trend",
  xlab = "Month",
  ylab = "Number of Bookings",
  las = 2,
  col = "skyblue"
)

#visulaization 9

hist(
  hotel$adr,
  breaks = 50,
  main = "Distribution of Average Daily Rate",
  xlab = "Average Daily Rate (ADR)",
  ylab = "Frequency",
  col = "skyblue"
)

#visualization 10

cor_data <- hotel[, c(
  "is_canceled",
  "lead_time",
  "stays_in_weekend_nights",
  "stays_in_week_nights",
  "adults",
  "children",
  "adr",
  "previous_cancellations",
  "booking_changes",
  "total_of_special_requests"
)]

cor_matrix <- cor(
  cor_data,
  use = "complete.obs"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7,
  title = "Correlation Heatmap",
  mar = c(0, 0, 2, 0)
)