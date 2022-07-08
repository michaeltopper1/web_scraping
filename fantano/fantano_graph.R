library(tidyverse)
library(lubridate)
library(ggtext)
library(ggthemes)
library(plotly)
fantano <- read_csv("fantano/xxmaster_data.csv")

fantano %>% 
  select(user_score, fantano_rating)

fantano_graph <- fantano %>% 
  mutate(release_date = mdy(release_date),
         release_year = year(release_date)) %>% 
  mutate(across(ends_with("score"), ~as.double(.))) %>% 
  mutate(genres = str_to_lower(genres)) %>% 
  mutate(hip_hop = ifelse(str_detect(genres,"hip hop|rap"), 1, 0),
         user_higher = ifelse(user_score > fantano_rating, "User Rating Higher", "User Rating Lower"),
         other_critic_higher = ifelse(critic_score > fantano_rating, "Fantano Rated Lower", "Fantano Rated Higher")) %>% 
  filter(hip_hop == 1 & release_year > 2009) %>% 
  arrange(desc(number_user_reviews)) %>% 
  head(100) %>% 
  mutate(average_fantano = mean(fantano_rating, na.rm = T),
         average_critic = mean(critic_score, na.rm = T),
         average_user = mean(user_score, na.rm = T))

test <- fantano_graph %>% 
  ggplot(aes(release_date, fantano_rating, text = paste(" Album:",album,"\n",
                                                        "Artist:", artist, "\n",
                                                        "Average Critic Score:", critic_score
                                                        ), color = other_critic_higher)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_hline(aes(yintercept = average_critic), linetype = "dashed", color = "#016392") +
  geom_hline(aes(yintercept = average_fantano), linetype = "dashed", color = "#c72e29") +
  annotate("text", x = as_date("2010-04-10"), y = 76, label = "Mean Critic Score",
           size  =3) +
  annotate("text", x = as_date("2010-04-10"), y = 68, label = "Mean Fantano Score",
           size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as_date("2010-01-01"), as_date("2022-008-01"))) +
  scale_y_continuous(breaks = c(20, 30, 40, 50 ,60, 70, 80, 90, 100)) +
  scale_color_wsj() +
  labs(color = "Relative to Other Critics:", y = "Anthony Fantano Rating (Multiplied by 10)", x = "Album Release Date", 
       title = "Anthony Fantano's Ratings of the 100 Most Popular Hip Hop/Rap Albums",
       subtitle = "Popularity is measured based on the number of user reviews. Time spans from January 2010-July 2022") +
  theme_minimal() +
  theme(legend.position = "bottom") 






ggplotly(test, tooltip = "text") %>% 
  layout(legend = list(orientation = "h"),
         title = list(text = paste0('Anthony Fantano\'s Ratings of the 100 Most Popular Hip Hop/Rap Albums',
                                    '<br>',
                                    '<sup>',
                                    'Popularity is measured based on the number of user reviews. Time spans from January 2010-July 2022','</sup>')))

# scales::show_col(wsj_pal()(12))
