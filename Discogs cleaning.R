# Discogs data cleaning and merging with Pitchfork
library(tidyverse)
library(plyr)
library(stringr)
library(sjmisc)
library(stringdist)

# Append all CSVs together
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis/discogs_data")
discogs_df <- ldply(list.files(),
                    read.csv, header=TRUE)
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis")




# Append all of the 'missing' Discogs csvs
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis/discogs_dataMissing")
discogsMissing_df <- ldply(list.files(),
                    read.csv, header=TRUE)
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis")


# Drop all unsuccessful searches
discogsMissing_df <- discogsMissing_df %>% 
  drop_na(X2)

# Merge with the Discogs data and drop duplicates
discogs_df <- discogs_df %>% 
  bind_rows(discogsMissing_df) %>% 
  arrange(rowSums(is.na(.))) %>%  # sort rows by number of NAs
  distinct(X1, .keep_all = TRUE) 
  

# Get year - year can appear in one of three columns
year_vector <- as.character(c(1900:2022))

discogsYear_df <- discogs_df %>% 
  select(X1, X5, X6, X7) %>% 
  mutate(yearCol5 = str_extract(X5, paste(year_vector, collapse = "|"))) %>% 
  mutate(yearCol6 = str_extract(X6, paste(year_vector, collapse = "|"))) %>% 
  mutate(yearCol7 = str_extract(X7, paste(year_vector, collapse = "|"))) %>% 
  select(X1, yearCol5, yearCol6, yearCol7) %>% 
  mutate(publish_yearDiscogs = if_else(!is.na(yearCol5), yearCol5,
                                if_else(!is.na(yearCol6),
                                        yearCol6, yearCol7))) %>% 
  select(X1, publish_yearDiscogs)

# Merge back into Discogs data
discogs_df <- discogs_df %>% 
  left_join(discogsYear_df, by = ("X1"))




# Pitchfork df containing one row per album
pitchfork_album_df <- review_dataframe_clean %>% 
  group_by(artist_name, genre_name) %>% 
  # For each artist, single row with all the artist's genres 
  dplyr::mutate(row_number = row_number()) %>% 
  dplyr::mutate(genre_occurances = max(row_number)) %>% 
  arrange(desc(genre_occurances)) %>% 
  group_by(artist_name) %>% 
  dplyr::mutate(genre_name = if_else(row_number==1, genre_name, "")) %>% 
  dplyr::mutate(artist_genres = paste0(genre_name, collapse = " ")) %>%
  dplyr::mutate(artist_genres = str_squish(artist_genres)) %>% 
  dplyr::mutate(artist_genres = gsub(" ", " | ", artist_genres)) %>% 
  ungroup() %>% 
  unite(pitchfork_artist_album, artist_name, album_name, sep = " – ", remove = FALSE) %>% 
  distinct(pitchfork_artist_album, .keep_all = TRUE) %>% 
  # Collaboration artist names are split on Pitchfork data, join to match Discogs
  unite(pitchfork_album_date, album_name, date_time, sep = " - ", remove = FALSE) %>% 
  group_by(pitchfork_album_date) %>% 
  dplyr::mutate(artist_name_new = paste(artist_name, collapse = ", ")) %>% 
  ungroup() %>% 
  # mutate(artist_name = artist_name_new) %>% 
  select(-pitchfork_album_date) 


# Merge Pitchfork data with Discogs data
review_dataframe_revised <- pitchfork_album_df %>% 
  unite(X1, artist_name, album_name, sep = " ", remove = FALSE) %>% 
  # Remove duplicates of artist_album name
  left_join(discogs_df, by = "X1") %>% 
  # Remove all non-numerics and spaces
  unite(pitchfork_artist_album, artist_name_new, album_name, sep = " – ", remove = FALSE) %>% # Need to redo this with the new names 
  mutate_at(c("X2", "pitchfork_artist_album"), ~str_replace_all(., "[^[:alnum:]]", "")) %>% 
  mutate_at(c("X2", "pitchfork_artist_album"), ~tolower(.)) %>% 
  mutate(stringdistVal = stringdist(X2, pitchfork_artist_album)) %>% 
  # Replace Pitchfork publish year with Discogs publish year when stringdist<5
  mutate(publish_year = if_else(stringdistVal<5 & publish_year>as.numeric(publish_yearDiscogs),
                                as.numeric(publish_yearDiscogs), publish_year)) %>% 
  select(artist_name, album_name, publish_year, genre_name, album_rating, date_time, artist_genres) 


write.csv(review_dataframe_revised, "Pitchfork_data_cleaned.csv")






  
  
