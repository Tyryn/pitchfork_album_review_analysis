# Pitchfork analysis

library(tidyverse)
library(wesanderson)
library(viridis)

# Create grouped bar graph of number of reviews per genre per year
number_reviews <- review_dataframe %>%
  group_by(genre_name, publish_year) %>%
  add_tally() %>% 
  select(genre_name, publish_year, n) %>% 
  distinct(genre_name, publish_year, .keep_all = TRUE) %>% 
  # Reshape wide so that the missing genres are added
  spread(genre_name, n) %>% 
  gather(Genre, `Number reviews`, Electronic:`<NA>`)  %>% 
  mutate(`Number reviews` = replace_na(`Number reviews`, 0)) 


# Bar graph of the total reviews per year
number_reviewsYear <- number_reviews %>%
  group_by(publish_year) %>% 
  mutate(reviews = sum(`Number reviews`)) %>% 
  distinct(publish_year, .keep_all = TRUE)

plot <- ggplot(number_reviewsYear, aes(x=publish_year, y=reviews)) +
  geom_bar(stat = "identity", fill = "#FF6666") + 
  scale_x_continuous("", breaks=seq(1998, 2021, 2)) +
  scale_y_continuous("Number of reviews") + 
  geom_text(aes(label=reviews), vjust=-0.5, size=3)
plot

# Bar graph with only the four main genres and "other"
number_reviewsAbbr <- number_reviews %>% 
  mutate(Genre = replace(Genre, Genre!="Electronic" & Genre!="Experimental" &
                           Genre!="Rap" & Genre!="Rock", "Other")) %>% 
  group_by(Genre, publish_year) %>% 
  mutate(reviews_genre = sum(`Number reviews`)) %>% 
  group_by(publish_year) %>% 
  mutate(reviews_year = sum(`Number reviews`)) %>% 
  mutate(genre_prop = reviews_genre/reviews_year) %>% 
  distinct(publish_year, Genre, genre_prop, reviews_genre) 
  

# Want to set the levels so that the order of the bars looks neat
number_reviewsAbbr$Genre <- factor(number_reviewsAbbr$Genre, 
                                   levels = c("Other", "Experimental",
                                              "Rap","Electronic", "Rock"))
  

plot <- ggplot(number_reviewsAbbr, aes(x=publish_year, y=genre_prop,
                                   fill=Genre)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous("", breaks=seq(1998, 2021, 2)) +
  scale_y_continuous("") + 
  coord_flip() + scale_fill_manual(values=wes_palette(n=5,name="FantasticFox1")) +
  geom_text(aes(label=ifelse(genre_prop >= 0.05, paste0(sprintf("%.0f", genre_prop*100),"%"),"")),
            position=position_stack(vjust=0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot

# Bar plot of the "Other" genres
number_reviewsAbbrOther <- number_reviews %>% 
  group_by(publish_year) %>% 
  mutate(reviews_year = sum(`Number reviews`)) %>% 
  filter(Genre!="Electronic" & Genre!="Experimental" &
                           Genre!="Rap" & Genre!="Rock") %>% 
  group_by(Genre, publish_year) %>% 
  mutate(reviews_genre = sum(`Number reviews`)) %>% 
  mutate(genre_prop = reviews_genre/reviews_year) %>% 
  distinct(publish_year, Genre, genre_prop, reviews_genre) %>% 
  mutate(Genre = replace(Genre, Genre=="<NA>", "Unspecified"))

plot <- ggplot(number_reviewsAbbrOther, aes(x=publish_year, y=genre_prop,
                                       fill=Genre)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_continuous("", breaks=seq(1998, 2021, 2)) +
  scale_y_continuous("") +
  coord_flip() +
  geom_text(aes(label=ifelse(genre_prop >= 0.07, paste0(sprintf("%.0f", genre_prop*100),"%"),"")),
            position=position_stack(vjust=0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot

# Create line graph that shows moving average review at date per genre
line_data <- review_dataframe %>% 
  filter(genre_name=="Rock" | genre_name=="Electronic" | genre_name=="Pop/R&B" | 
           genre_name=="Rap") 

plot_line <- ggplot(line_data, aes(x=date_time, y=album_rating, color=genre_name)) +
  geom_smooth(se=FALSE) +
  scale_y_continuous("Album rating") 

plot_line


# Line graph for other genres
line_data_other <- review_dataframe %>% 
  filter(genre_name!="Rock" & genre_name!="Electronic" & genre_name!="Pop/R&B" & 
           genre_name!="Rap") 

plot_line_other <- ggplot(line_data_other, aes(x=date_time, y=album_rating, color=genre_name)) +
  geom_smooth(se=FALSE) +
  scale_y_continuous("Album rating") 

plot_line_other


# Create table of top 5 albums by genre
top_df <- review_dataframe %>% 
  # Got to remove all of the albums that are actually collections etc.
  filter(album_name!="Purple Rain Deluxe — Expanded Edition") %>% 
  group_by(genre_name) %>% 
  slice_max(order_by = album_rating, n = 5) %>% 
  select(artist_name, album_name, publish_year, album_rating) %>% 
  distinct(artist_name, album_name, .keep_all = TRUE)
  

# Create table of bottom 5 albums by genre
bottom_df <- review_dataframe %>% 
  group_by(genre_name) %>% 
  slice_min(order_by = album_rating, n = 5)  %>% 
  select(artist_name, album_name, publish_year, album_rating) %>% 
  distinct(artist_name, album_name, .keep_all = TRUE)


# Artists with 3 or more albums with highest average album rating
top_average_df <- review_dataframe %>% 
  group_by(artist_name) %>% 
  distinct(artist_name, album_name, .keep_all = TRUE) %>% 
  dplyr::mutate(n=max(row_number())) %>% 
  filter(n>2) %>% 
  arrange(artist_name) %>% 
  dplyr::mutate(rating_average = mean(album_rating)) %>% 
  # Create single row with album names (only top three with year)
  mutate(publish_year_brackets = paste0("(", publish_year, ")")) %>% 
  unite(album_year, album_name, publish_year_brackets, sep = " ") %>% 
  arrange(desc(album_rating)) %>%
  slice(1:3) %>% 
  dplyr::mutate(top3_albums = paste0(album_year, collapse = " | ")) %>% 
  distinct(artist_name, .keep_all = TRUE) %>% 
  arrange(desc(rating_average)) %>% 
  select(artist_name, top3_albums, artist_genres, n, rating_average) 





# Artists with 3 or more albums with lowest average album rating
bottom_average_df <- review_dataframe %>% 
  group_by(artist_name) %>% 
  distinct(artist_name, album_name, .keep_all = TRUE) %>% 
  mutate(n=max(row_number())) %>% 
  filter(n>2) %>% 
  arrange(artist_name) %>% 
  mutate(rating_average = mean(album_rating)) %>% 
  distinct(artist_name, .keep_all = TRUE) %>% 
  arrange(rating_average) %>% 
  select(artist_name, genre_name, n, rating_average)



# Function that ranks each genre by average rating
genreRanking.func <- function(data.frame, rank.number){
  ranking_df <- review_dataframe %>% 
    group_by(genre_name) %>% 
    summarise(rating_mean = mean(album_rating)) %>% 
    filter(genre_name!="Unspecified") %>% 
    arrange(desc(rating_mean))
  
  genre_ranking <- paste0(as.character(ranking_df[rank.number,1]), " ", 
                          "(", paste0(round(ranking_df[rank.number,2],2)), ")")
}

genre_1 <- genreRanking.func(review_dataframe, 1)
genre_2 <- genreRanking.func(review_dataframe,2)

percent_ranking <- paste0(round(ranking_df[1,2],2))


prop <- review_dataframe %>% 
  group_by(genre_name) %>%
  summarise(n=n()) %>% 
  mutate(freq = paste0(round((n/sum(n))*100, 2), "%")) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(genre_name = replace_na(genre_name, "Unspecified"))

prop_rank <- paste0(as.character(prop[1,1]), " ", "(",prop[1,3], ")")


# Worst electronic music album
review_dataframe_shite <- review_dataframe %>% 
  group_by(genre_name) %>% 
  filter(publish_year>=2010 & publish_year<=2013) %>% 
  arrange(-album_rating)