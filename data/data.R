library(dplyr)
library(readr)

# Load data
full_data <- read_csv("DelayedFlights.csv")

# Split data
q1_data <- full_data %>%
  filter(Month < 4)

q2_data <- full_data %>%
  filter(Month >= 4, Month < 7)

q3_data <- full_data %>%
  filter(Month >= 7, Month < 10)

q4_data <- full_data %>%
  filter(Month >= 10)

