library(readr)
library(dplyr)
library(ggplot2)

# Load data
data_path <- file.path(getwd(), "data", "DelayedFlights.csv")
data <- read_csv(data_path)

# Manipulate data
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data <- data %>%
  mutate(WeekDay = ifelse(DayOfWeek < 5, "Workday", "Weekend") %>% as.factor(),
         Month = factor(Month, levels = 1:12, labels = months),
         UniqueCarrier = as.factor(UniqueCarrier))

# Calculate averages
avg_months_arr <- data %>%
  group_by(Month) %>%
  summarize(avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
  mutate(delay = "arrival")

avg_months_dep <- data %>%
  group_by(Month) %>%
  summarize(avg_delay = mean(DepDelay, na.rm = TRUE)) %>%
  mutate(delay = "departure")

avg_months <- avg_months_dep %>%
  rbind(avg_months_arr)

# Plot averages
plot_months <- ggplot(avg_months, aes(Month, avg_delay, fill = delay)) + 
  geom_col(position = position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("#F5403D", "#C20D0A")) + 
  labs(title = "Average delay vs month",
       y = "Average delay") +
  theme_bw()

# Saving plots
ggsave("avg_months.png", plot_months)

# Calculate cancelled flights
canc_flights_months <- data %>%
  group_by(Month) %>%
  summarize(n = sum(Cancelled))

# Plot cancelled flights 
plot_cancelled <- ggplot(canc_flights_months, aes(Month, n)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Number of cancelled flights vs months",
       y = "Cancelled flights") +
  theme_bw()

# Saving plot
ggsave("cancelled_flights.png", plot_cancelled)

# Creating workday-weekend percentage plot
wd_wk_delays <- data %>%
  group_by(WeekDay) %>%
  summarize(delayed_percentage = sum(ArrDelay > 60, na.rm = TRUE) / nrow(data))

wd_wk_plot <- ggplot(wd_wk_delays, aes(reorder(WeekDay,
                                               -delayed_percentage), delayed_percentage)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Delayed flights percentage vs weekday",
       x = "Weekday",
       y = "Percentage") + 
  theme_bw()

ggsave("weekday_perc.png", wd_wk_plot)
