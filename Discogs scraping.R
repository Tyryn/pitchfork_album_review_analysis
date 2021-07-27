
#################
# Further scraping to make sure that the dataset contains only albums that are not reissues, vinyls etc. 

# Using Rselenium on discogs

library(RSelenium)
library(wdman)
library(plyr)
library(dplyr)
library(pbapply)
library(rvest)

# Vector of artist names and albums to be passed into loop 
search_list <- review_dataframe %>% 
  unite(search_term, artist_name, album_name, sep = " ") %>% 
  distinct(search_term)

search_list <- search_list[["search_term"]]

# The below so that I can loop this operation and save it in CSVs and when it breaks its not the end of the world
search_list_length <- c(1:length(search_list)) # 301 is the most recent csv saved
search_list_chunks <- split(search_list_length, ceiling(seq_along(search_list_length)/20))

# Selenium set up, using firefox
rD <- rsDriver(browser="firefox", port = 5207L)
remDr <- rD[["client"]]

# rD[["server"]]$stop()
# rm(rD)
# gc(rD)
# remDr$close()
# remDr$closeServer()
# # 
# rm(rD, remDr)

# remDr$setWindowSize(width = 1500, height = 1000)

for(search_list_chunk in search_list_chunks) {
  
  # Create search list for artists and albums to be saved in batches of 50 to csvs
  search_list_abbr <- search_list[min(search_list_chunk):max(search_list_chunk)]
  
  # Selenium code to navigate Discogs
  selenium_list <- pblapply(search_list_abbr, function(x){
    remDr$navigate("https://www.discogs.com/")
    
    try_backoff(remDr$findElement(using = "id", value = "search_q")$sendKeysToElement(list(x)))
    try_backoff(remDr$findElements("id", "do_site_search")[[1]]$clickElement())
    
    try_backoff(remDr$findElements("class", "link_with_count")[[1]]$clickElement())
    
    # Function to remove pop-up banner at the bottom of the screen
    tryCatch(remDr$findElements(using = "id", "move")[[1]]$clickElement(), 
             error = function(e) {
               return(e)
             })
    
    tryCatch(remDr$findElements("class", "search_result_title")[[1]]$clickElement(),
             error = function(e) {
               return(e)
             })
    Sys.sleep(1)
    
    # Rvest code to collect album name, artist name, and year
    
    html <- remDr$getPageSource()[[1]]
    
    artist_album_name <- read_html(html) %>% 
      html_nodes(".header_3FV2N") %>% 
      html_text()
    
    album_date <- read_html(html) %>% 
      html_nodes(".content_3IW3p") %>% 
      html_text() 
    
    vector <- c(x, artist_album_name, album_date)
    
    vector
    
  })
  
  # Convert to data frame
  data.frame <- plyr::ldply(selenium_list, rbind) 
  
  write.csv(data.frame, paste0("discogs_data/data_frame", "_", min(search_list_chunk), ".csv"), row.names = FALSE)
  
  
}



# Append all CSVs together
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis/discogs_data")
discogs_df <- ldply(list.files(),
                               read.csv, header=TRUE)
setwd("C:/Users/DELL/Documents/Pitchfork review analysis/pitchfork_album_review_analysis")


# Get all rows where data were not collected
search_listMissing <- discogs_df %>% 
  mutate(missing_tag = if_else(is.na(X2), 1, 0)) %>% 
  filter(missing_tag==1) %>% 
  select(X1)

search_listMissing <- search_listMissing[["X1"]]

search_list_lengthMissing <- c(1:length(search_listMissing)) # 301 is the most recent csv saved
search_list_chunksMissing <- split(search_list_lengthMissing, ceiling(seq_along(search_list_lengthMissing)/20))


# Selenium set up, using firefox
rD <- rsDriver(browser="firefox", port = 5209L)
remDr <- rD[["client"]]

# rD[["server"]]$stop()
# rm(rD)
# gc(rD)
# remDr$close()
# remDr$closeServer()
# #
# rm(rD, remDr)

# remDr$setWindowSize(width = 1500, height = 1000)

for(search_list_chunk in search_list_chunksMissing) {
  
  # Create search list for artists and albums to be saved in batches of 50 to csvs
  search_list_abbr <- search_listMissing[min(search_list_chunk):max(search_list_chunk)]
  
  # Selenium code to navigate Discogs
  selenium_list <- pblapply(search_list_abbr, function(x){
    remDr$navigate("https://www.discogs.com/")
    
    try_backoff(remDr$findElement(using = "id", value = "search_q")$sendKeysToElement(list(x)))
    try_backoff(remDr$findElements("id", "do_site_search")[[1]]$clickElement())
    
    try_backoff(remDr$findElements("class", "link_with_count")[[1]]$clickElement())
    
    # Function to remove pop-up banner at the bottom of the screen
    tryCatch(remDr$findElements(using = "id", "move")[[1]]$clickElement(), 
             error = function(e) {
               return(e)
             })
    
    tryCatch(remDr$findElements("class", "search_result_title")[[1]]$clickElement(),
             error = function(e) {
               return(e)
             })
    Sys.sleep(1)
    
    # Rvest code to collect album name, artist name, and year
    
    html <- remDr$getPageSource()[[1]]
    
    artist_album_name <- read_html(html) %>% 
      html_nodes(".header_3FV2N") %>% 
      html_text()
    
    album_date <- read_html(html) %>% 
      html_nodes(".content_3IW3p") %>% 
      html_text() 
    
    vector <- c(x, artist_album_name, album_date)
    
    vector
    
  })
  
  # Convert to data frame
  data.frame <- plyr::ldply(selenium_list, rbind) 
  
  write.csv(data.frame, paste0("discogs_dataMissing/data_frame", "_", min(search_list_chunk), ".csv"), row.names = FALSE)
  
  
}





