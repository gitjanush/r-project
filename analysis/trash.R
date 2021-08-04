avg_airports <- data %>%
  group_by(Origin) %>%
  summarize(avg_dep_delay = mean(DepDelay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

avg_airports_top <- avg_airports %>%
  head(10)

avg_airlines <- data %>%
  group_by(UniqueCarrier) %>%
  summarize(avg_dep_delay = mean(DepDelay, na.rm = TRUE)) %>%
  arrange(desc(avg_airlines))

plot_airports_top10 <- ggplot(avg_airports_top, aes(x = reorder(Origin,
                                                                -avg_dep_delay),
                                                    y = avg_dep_delay)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Average delay vs origin airport",
       x = "Origin airport",
       y = "Average delay") +
  theme_bw()

plot_airlines <- ggplot(avg_airlines, aes(x = reorder(UniqueCarrier,
                                                      -avg_dep_delay),
                                          y = avg_dep_delay)) +
  geom_col(fill = "#C20D0A") +
  labs(title = "Average delay vs airline",
       x = "Airline",
       y = "Average delay") +
  theme_bw()