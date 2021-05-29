# Pitchfork analysis

library(tidyverse)

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

plot <- ggplot(number_reviews, aes(x=publish_year, y=`Number reviews`,
                                   fill=Genre)) +
  geom_bar(position="stack", stat="identity")
plot



# Create line graph that shows moving average review at date per genre


