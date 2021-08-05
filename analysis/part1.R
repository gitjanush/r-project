library(readr)
library(dplyr)
library(ggplot2)

### Load data
data_path <- file.path(getwd(), "data", "DelayedFlights.csv")
data <- read_csv(data_path)

data <- data %>%
  mutate(Month = as.factor(Month))

### Delay vs month
## Calculate averages
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

## Plot averages
plot_months <- ggplot(avg_months, aes(Month, avg_delay, fill = delay)) + 
  geom_col(position = position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("#F5403D", "#C20D0A")) + 
  labs(title = "Average delay vs month",
       y = "Average delay") +
  theme_bw()

## Saving plots
ggsave("img/avg_months.png", plot_months)





### Cancelled flights vs month
## Calculate
canc_flights_months <- data %>%
  group_by(Month) %>%
  summarize(n = sum(Cancelled))

## Plot
plot_cancelled <- ggplot(canc_flights_months, aes(Month, n)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Number of cancelled flights vs months",
       y = "Cancelled flights") +
  theme_bw()

## Save
ggsave("img/cancelled_flights.png", plot_cancelled)





### Working day / weekend vs delay
## Calculate
wd_wk_delays <- data %>%
  group_by(WeekDay) %>%
  summarize(delayed_percentage = sum(ArrDelay > 60, na.rm = TRUE) / nrow(data))

## Plot
wd_wk_plot <- ggplot(wd_wk_delays, aes(reorder(WeekDay,
                                               -delayed_percentage), delayed_percentage)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Delayed flights percentage vs weekday",
       x = "Weekday",
       y = "Percentage") + 
  theme_bw()

## Save
ggsave("img/weekday_perc.png", wd_wk_plot)





### Distance vs delay
## Calculate distance percentiles
three_groups <- quantile(data$Distance, c(0.33, 0.66))

## Label flights
hauls <- data %>%
  mutate(DistLabel = case_when(
    Distance < 700 ~ "short-haul",
    Distance < 3000 ~ "medium-haul",
    Distance >= 3000 ~ "long-haul")
  ) %>%
  mutate(DistLabel = factor(DistLabel, levels = c("short-haul",
                                                  "medium-haul", 
                                                  "long-haul")))

hauls_grouped <- hauls %>%
  group_by(DistLabel) %>%
  summarize(avg_delay = mean(ArrDelay, na.rm = TRUE))

hauls_cut <- hauls %>%
  filter(ArrDelay < 500, ArrDelay > 0) 

## Plot 1
hauls_plot <- ggplot(hauls_cut, aes(ArrDelay)) + 
  geom_density(color = "#C20D0A") +
  facet_wrap(. ~ DistLabel) + 
  labs(title = "Arrival delay distribution vs distance",
       x = "Arrival delay (mins)",
       y = "") + 
  theme_bw()

## Plot 2
hauls_p2 <- ggplot(hauls_grouped, aes(x = DistLabel, y = avg_delay)) +
  geom_col(fill = "#C20D0A") + 
  labs(title = "Average arrival delay vs distance",
       x = "Flight",
       y = "Average arrival delay (min)") + 
  theme_bw()

## Save
ggsave("img/hauls_plot.png", hauls_plot)
ggsave("img/hauls_avgs.png", hauls_p2)

