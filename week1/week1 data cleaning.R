install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("corrplot")
install.packages("naniar")
library(tidyverse)
library(janitor)
library(skimr)
library(corrplot)
library(naniar)

hotel <- read.csv("Dataset/hotel_bookings.csv")

hotel$lead_time_normalized <-
  (hotel$lead_time - min(hotel$lead_time, na.rm = TRUE)) /
  (max(hotel$lead_time, na.rm = TRUE) -
     min(hotel$lead_time, na.rm = TRUE))

hotel$adr_normalized <-
  (hotel$adr - min(hotel$adr, na.rm = TRUE)) /
  (max(hotel$adr, na.rm = TRUE) -
     min(hotel$adr, na.rm = TRUE))
# Convert selected categorical variables to factors
hotel$hotel <- as.factor(hotel$hotel)
hotel$meal <- as.factor(hotel$meal)
hotel$market_segment <- as.factor(hotel$market_segment)
hotel$distribution_channel <- as.factor(hotel$distribution_channel)
hotel$deposit_type <- as.factor(hotel$deposit_type)
hotel$customer_type <- as.factor(hotel$customer_type)


encoded_data <- model.matrix(
  ~ hotel + meal + market_segment +
    distribution_channel + deposit_type +
    customer_type - 1,
  data = hotel
)
# Save cleaned dataset for Week 2
write.csv(
  hotel,
  "Output/hotel_bookings_cleaned.csv",
  row.names = FALSE
)