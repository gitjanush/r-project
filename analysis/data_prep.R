# Data prep
library(ggplot2)
library(readr)
library(dplyr)

# Load data
data_path <- file.path(getwd(), "data", "DelayedFlights.csv")
data <- read_csv(data_path)

# Manipulate data
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data <- data %>%
  mutate(WeekDay = ifelse(DayOfWeek < 5, "Working day", "Weekend") %>% as.factor(),
         Month = factor(Month, levels = 1:12, labels = months),
         UniqueCarrier = as.factor(UniqueCarrier))

# Cleaning data
ggplot(data, aes(x = 1, y = ArrDelay)) + geom_boxplot()

# Save data
write_csv(data, data_path)
