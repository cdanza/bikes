library(tidyverse)

# Import "day" as a dataframe named df
df <- read_csv("rawdata/day.csv")
df

# Everything is already labeled

# Export "day" to clean folder
write_csv(df, "data/clean_day.csv")