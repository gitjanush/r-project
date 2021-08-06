library(readr)
library(dplyr)
library(ggplot2)

### Load data
data_path <- file.path(getwd(), "data", "DelayedFlights.csv")
data <- read_csv(data_path)

data <- data %>%
  mutate(Month = as.factor(Month))

### Prep data
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data <- data %>%
  mutate(WeekDay = ifelse(DayOfWeek < 5, "Working day", "Weekend") %>% as.factor(),
         Delayed = ifelse(ArrDelay <= 60, "NO", "YES") %>% as.factor(),
         Month = factor(Month, levels = 1:12, labels = months),
         UniqueCarrier = as.factor(UniqueCarrier))




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
  rbind(avg_months_arr) %>%
  mutate(delay = factor(delay, levels = c("departure", "arrival")))

## Plot averages
plot_months <- ggplot(avg_months, aes(Month, avg_delay, fill = delay)) + 
  geom_col(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values=c("#F5403D", "#C20D0A")) + 
  labs(title = "Average delay vs month",
       y = "Average delay") +
  theme_bw()

plot_months
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





### The most crowded airports
## Find the most crowded airports (arrivals)
most_crowded_arrivals <- data %>%
  group_by(Dest) %>%
  summarize(n_of_arrivals = n()) %>%
  arrange(desc(n_of_arrivals))

## Find the most crowded airports (departures)
most_crowded_depart <- data %>%
  group_by(Origin) %>%
  summarize(n_of_departures = n()) %>%
  arrange(desc(n_of_departures))

## Find the most crowded airports (arrivals and departures combined)
most_crowded_airports <- most_crowded_arrivals %>%
  inner_join(most_crowded_depart, by = c("Dest" = "Origin")) %>%
  mutate(n_of_operations = n_of_arrivals + n_of_departures) %>%
  arrange(desc(n_of_operations)) %>%
  rename(Airport = Dest) %>%
  head(n=10)

## Plot
most_crowded_plot <- ggplot(most_crowded_airports, aes(reorder(Airport, -n_of_operations),
                                                       n_of_operations)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Number of operations vs airport",
       x = "Airport",
       y = "Number of operations") +
  theme_bw()

## Save
ggsave("img/most_crowded_airports.png", most_crowded_plot)




### Airport vs delay
## Find airports with the biggest percentage of delayed flights (arrivals)
top_delay <- data %>%
  group_by(Dest) %>%
  summarize(n_of_delayed_flights = sum(ArrDelay > 60, na.rm = TRUE),
            n_of_flights = sum(ArrDelay, na.rm = TRUE),
            perc_of_delayed_flights = round(n_of_delayed_flights/n_of_flights*100, 4)) %>%
  arrange(desc(perc_of_delayed_flights)) %>% 
  head(n=10)

## Plot
delay_airport_plot <- ggplot(top_delay, aes(reorder(Dest, -perc_of_delayed_flights),
                                            perc_of_delayed_flights)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Percent of delayed flights vs airports",
       x = "Airport",
       y = "Percent of delayed flights") +
  theme_bw()

## Save
ggsave("img/delay_airport_perc.png", delay_airport_plot)





### The most common cancellation reason
cancel_reason <- data %>%
  filter(Cancelled > 0) %>%
  group_by(CancellationCode) %>%
  summarize(n = n()) %>%
  arrange(desc(n))





### Taxi In/Out vs Delay
## Count averages
avg_taxi_out <- data %>%
  filter(Delayed %in% c("YES", "NO")) %>%
  group_by(Delayed) %>%
  summarize(avg_taxi = mean(TaxiOut, na.rm = TRUE)) %>%
  mutate(Taxi = "OUT")

avg_taxi_in <- data %>%
  filter(Delayed %in% c("YES", "NO")) %>%
  group_by(Delayed) %>%
  summarize(avg_taxi = mean(TaxiIn, na.rm = TRUE)) %>%
  mutate(Taxi = "IN")

avg_taxi_both <- avg_taxi_out %>%
  rbind(avg_taxi_in) %>%
  mutate(Taxi = factor(Taxi, levels = c("OUT", "IN")))

## Plot
taxis_vs_delay_plot <- ggplot(avg_taxi_both, aes(Delayed, avg_taxi, fill = Taxi)) +
  geom_col(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values=c("#F5403D", "#C20D0A")) + 
  labs(title = "Average taxi time vs delay",
       x = "Delayed",
       y = "Average taxi time") +
  theme_bw()

## Save
ggsave("img/taxis_vs_delayed.png", taxis_vs_delay_plot)
