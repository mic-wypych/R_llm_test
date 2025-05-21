library(tidyverse)
library(tidytext)
library(ggforce)


threads <- read.csv("data/threads.csv", fileEncoding = "latin1") 
threads_all <- read.csv("data/threads_all.csv", fileEncoding = "latin1") 
comments <- read.csv("data/comments.csv", fileEncoding = "latin1") 
#### some preprocessing ----

threads_tokenized <- threads  %>%
  mutate(id = 1:944) %>%
  select(id, title, text) %>%
  unnest_tokens(output = "word", input = "text", to_lower = TRUE) %>% 
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) #should I stem?


#### Basic descriptions plots ----

# n of characters

threads %>%
  mutate(length = stringr::str_length(text)) %>%
  ggplot(aes(x=length)) +
  geom_density()

#n of words

threads_tokenized %>%
  count(id) %>%
  ggplot(aes(x=n)) +
  geom_density()

#span in time: posts per month

threads %>%
  mutate(year_month =  format(date_utc, "%Y-%m")) %>%
  count(year_month) %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#scores

threads %>%
  ggplot(aes(x=score)) +
  geom_density()

#posts per user
threads %>%
  count(author) %>%
  ggplot(aes(x=n)) +
  geom_density()


## comments

threads %>%
  ggplot(aes(x=comments)) +
  geom_histogram(bins = 100) +
  geom_mark_hull(aes(label = "3 posts have over 2500 comments!", filter = comments > 2500, y = 0))


#### basic descriptions - numbers -----

threads

#### organising ----

#' How to organize this section?