library(tidyverse)
library(vtable)

# Import cleaned dataset as df
df <- read_csv("data/clean_day.csv")

# Display summary statistics from vtable package
sumtable(df)

# Fig 1. Distribution of Casual Users
ggplot(data = df, mapping = aes(x = casual)) +
  geom_histogram(binwidth = 100) +
  ylim(0, 80) +
  labs(title = "Distribution of Casual Users by Day",
       x = "Casual Users",
       y = "Count") +
  theme_bw()

# Fig. 2 Distribution of Registered Users
ggplot(data = df, mapping = aes(x = registered)) +
  geom_histogram(binwidth = 200) +
  ylim(0, 60) +
  labs(title = "Distribution of Registered Users by Day",
       x = "Registered Users by Day",
       y = "Count") +
  theme_bw()

#TESTING:

# Total Count histogram
# ~roughly normal distribution with 1000 bin width, tri-modal zooming in
ggplot(data = df, mapping = aes(x = cnt)) +
  geom_histogram(binwidth = 1000) +
  labs(title = "Distribution of Total Bikes Rented by Day",
       x = "Count of Bikes",
       y = "Count") +
  theme_bw()

# qq plot to test normality
ggplot(df, aes(sample = cnt))  +
  geom_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot for Total Bikes Rented by  Day",
     x = "Theoretical",
     y = "Sample") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Correlations
cor(df$cnt, df$windspeed)

