library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(pbapply)

# Function used in the webscraping code in case of internet interruption, mistake in code etc. 
try_backoff <- function(expr, silent = FALSE, max_attempts = 5, verbose = FALSE) {
  for (attempt_i in seq_len(max_attempts)) {
    results <- try(expr = expr, silent = silent)
    if (inherits(results, "try-error")) {
      backoff <- runif(n = 1, min = 0, max = 2^attempt_i - 1)
      if (verbose) {
        message("Backing off for ", backoff, " seconds.")
      }
      Sys.sleep(backoff)
    } else {
      if (verbose) {
        message("Succeeded after ", attempt_i, " attempts.")
      }
      break
    }
  }
  results
}

# Create function that will try read a page URL up to five times, thereafter it will break the loop
number_list <- as.character(1:1966)

review_data_list <- pblapply(number_list, function(i) {
  page <- paste0("https://pitchfork.com/reviews/albums/?page=", i)
    html <- try_backoff(read_html(page))
    # Below if else statement is to make sure that if the page doesn't exist, the loop will skip
    if(is.list(html)==TRUE){

  # 1. Extract all the weblinks on the page
  html_link <- html %>%
    html_nodes("a") %>% html_attr("href")
  
  # 2. Keep the links that are normal reviews
  review_link <- html_link[grepl("/albums/", html_link)]
  review_link <-  review_link[!grepl("?genre", review_link)]
  review_link <-  review_link[!grepl("/staff/", review_link)]
  review_link <-  review_link[!grepl("/popular/", review_link)]
  review_link <-  review_link[!grepl("/best/", review_link)]
  review_link <- review_link[!grepl("^/reviews/albums/$", review_link)]
  
  dataframe_list <- lapply(review_link, function(x) {
    # Page data ####
    review_url <- paste0("https://pitchfork.com", x)
    tryCatch(
      review <- read_html(review_url),
      error = function(e) {
        NA
      }
    )
    if(is.list(review)==TRUE) {

    
    # Extract each element ####
    # Artist name
    artist_name <- review %>%
      html_nodes(".single-album-tombstone__headings") %>%
      html_nodes("a") %>%
      html_text()
    
    # Album name
    album_name <- review %>%
      html_nodes(".single-album-tombstone__review-title") %>%
      html_text()
    
    # Date
    date_time <- review %>%
      html_nodes('time') %>%
      html_attrs() %>%
      # Extract the second element
      map(2) %>%
      unlist()
    
    # Rating
    album_rating <- review %>%
      html_nodes(".score-box") %>%
      html_nodes(".score-circle") %>%
      html_nodes(".score") %>%
      html_text
    
    # Genre
    genre_name <- review %>%
      html_nodes(".genre-list__item") %>%
      html_nodes("a") %>%
      html_text()
    
    # Record label
    label_name <- review %>%
      html_nodes(".single-album-tombstone__art") %>%
      html_nodes(".single-album-tombstone__meta") %>%
      html_nodes(".labels-list__item") %>%
      html_text()
    
    # Year published
    publish_year <- review %>%
      html_nodes(".single-album-tombstone__art") %>%
      html_nodes(".single-album-tombstone__meta") %>%
      html_nodes(".single-album-tombstone__meta-year") %>%
      html_text()
    
    data_frame <-
      data.frame(
        cbind(
          artist_name,
          album_name,
          date_time,
          genre_name,
          album_rating,
          publish_year,
          label_name
        )
      )
    
    return(data_frame)
  }
    else{return()}
  })
  # Merge into single dataframe ####
  data_frame_merged <- bind_rows(dataframe_list)
  print(head(data_frame_merged))
  print(i)
  print(Sys.time())
  return(data_frame_merged)
  
    }
    else{return()}
})

review_data_list[1]

# Coerce into a single dataframe
review_dataframe<- bind_rows(review_data_list) 


write.csv(review_dataframe, "Pitchfork_data_raw.csv")







