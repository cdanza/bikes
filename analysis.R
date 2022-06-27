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

# Total Count histogram
# ~ roughly normal distribution with 1000 bin width, tri-modal zooming in
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
cor(df$cnt, df$hum)
cor(df$cnt, df$temp)
cor(df$cnt, df$windspeed)
cor(df$cnt, df$weathersit)
cor(df$cnt, df$mnth)

# Scatter of (x) and cnt
ggplot(df, aes(x = hum, y =  cnt))  +
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(df, aes(x = temp, y =  cnt))  +
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(df, aes(x = windspeed, y =  cnt))  +
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(df, aes(x = weathersit, y =  cnt))  +
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(df, aes(x = mnth, y =  cnt))  +
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Histogram of (x) to check normality
ggplot(df, aes(x = hum))  +
  geom_histogram(binwidth = .01) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Linear models: Backward stepwise regression -- lm3 is best fit
lm1 <- lm(cnt ~ hum + temp + windspeed + weathersit + mnth + holiday + weekday + workingday, df)
summary(lm1)

lm2 <- lm(cnt ~ hum + temp + windspeed + weathersit + mnth + weekday, df)
summary(lm2)

lm3 <- lm(cnt ~ hum + temp + windspeed + weathersit + mnth + weekday + holiday, df)
summary(lm3)

lm4 <- lm(cnt ~ hum + temp + windspeed + weathersit + mnth + weekday + workingday, df)
summary(lm4)


