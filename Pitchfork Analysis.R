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
  geom_bar(stat = "identity") + 
  scale_x_continuous("", breaks=seq(1998, 2021, 2)) +
  scale_y_continuous("Number of reviews") + 
  geom_text(aes(label=reviews), vjust=-0.5)
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
  geom_text(aes(label=ifelse(genre_prop >= 0.07, paste0(sprintf("%.0f", genre_prop*100),"%"),"")),
            position=position_stack(vjust=0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot

# Bar plot of the "Other" genres
number_reviewsAbbrOther <- number_reviews %>% 
  filter( Genre!="Electronic" & Genre!="Experimental" &
                           Genre!="Rap" & Genre!="Rock") %>% 
  group_by(Genre, publish_year) %>% 
  mutate(reviews_genre = sum(`Number reviews`)) %>% 
  group_by(publish_year) %>% 
  mutate(reviews_year = sum(`Number reviews`)) %>% 
  mutate(genre_prop = reviews_genre/reviews_year) %>% 
  distinct(publish_year, Genre, genre_prop, reviews_genre) %>% 
  mutate(Genre = replace(Genre, Genre=="<NA>", "Unspecified"))

plot <- ggplot(number_reviewsAbbrOther, aes(x=publish_year, y=genre_prop,
                                       fill=Genre)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous("", breaks=seq(1998, 2021, 2)) +
  scale_y_continuous("") +
  coord_flip() +
  geom_text(aes(label=ifelse(genre_prop >= 0.07, paste0(sprintf("%.0f", genre_prop*100),"%"),"")),
            position=position_stack(vjust=0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot



# Create line graph that shows moving average review at date per genre


