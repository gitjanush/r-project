library(dplyr)
library(readr)

# Load data
full_data <- read_csv("DelayedFlights.csv")

# Split data
for (i in 1:12) {
  full_data %>%
    filter(Month == i) %>%
    write_csv(paste("month_", i, ".csv", sep = ""))
}

