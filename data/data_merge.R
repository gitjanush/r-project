library(dplyr)
library(readr)

# Create a vector of file names to join
fnames <- paste("month_", 1:12, ".csv", sep = "")

complete_data <- read_csv(fnames[1])

# Join in a loop
for (f in fnames[2:12]) {
  current_part <- read_csv(f)
  
  complete_data <- rbind(complete_data, current_part)
}

# Save the result
write_csv(complete_data, "DelayedFlights.csv")
