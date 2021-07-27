library(tidyverse)

# Remove the bullet, keep observations that convert to date time, create variables
review_dataframe_csv <- read.csv("Pitchfork_data_raw.csv")

review_dataframe_clean <- review_dataframe_csv %>%
  select(-X) %>%
  mutate(publish_year = parse_number(as.character(publish_year))) %>%
  arrange(desc(-publish_year)) %>%
  mutate(date_time = gsub("\\T.*","", date_time)) %>%
  mutate(date_time = as.Date(date_time)) %>%
  mutate(year = format(date_time, format = "%Y")) %>%
  filter((as.numeric(year)-as.numeric(publish_year))<2) %>%  # This is remove retrospective reviews
  select(-year) %>%
  mutate(album_rating = as.numeric(album_rating)) 
  


