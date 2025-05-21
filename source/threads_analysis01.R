#### Analysis plan ####

# 1. Descriptives 
#' Which descriptives will be needed?
#' - Length of main posts (number of words/characters?) DONE
#' - Dates for main posts and their frequency DONE
#' - Scores for main posts
#' - number of comments per main posts
#' - posts per users?
#' 
#' - Length of comments
#' - dates of comments
#' - scores of comments
#' - comments per user?
#' - Emotions in posts?
#' 
#' 
#' 2. What topics are in revenge stories?
#' - Prepare data for an LDA: tokenize, remove stopwords etc
#' - Run a series of LDAs and find optimal number of topics
#' - Review and plot the topics
#' - Analyze titles?
#' 
#' 2.5 Run a sentiment analysis of revenge stories?
#' 
#' 3. How are various topics related to engagement and upvotes?
#' - how do topics and other features like length etc. predict engagement?
#' 
#' 4. Put this into an app?


#### load and prepare data ----
library(tidyverse)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(broom)
library(showtext)
library(ggforce)
library(furrr)
#load data
threads <- read.csv("data/threads.csv", fileEncoding = "latin1") 

font_add_google("Oswald")
showtext_auto()

#set ggplot2 theme
theme_set(theme_minimal(base_size = 15, base_family = "Oswald"))



#tokenize
threads_tokenized <- threads  %>%
  mutate(id = 1:940) %>%
  select(id, title, text) %>%
  unnest_tokens(output = "word", input = "text", to_lower = TRUE) %>% 
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) #should I stem?



#### Basic descriptions ----

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
  mutate(year_month =  format(as.Date(date), "%Y-%m")) %>%
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
  geom_density() +
  geom_mark_hull(aes(label = "3 posts have over 2500 comments!", filter = comments > 2500, y = 0))




#### run LDA -------
# 
# #prepare document term matrix
# threads_dtm <- threads_tokenized %>%
#   group_by(id) %>%
#   count(word) %>%
#   cast_dtm(document = id, term = word, value = n)
# 
# #run the lda model
# lda_1 <- topicmodels::LDA(threads_dtm, k = 5, method = "Gibbs", control=list(alpha=1, delta = .1, seed = 123))
# 
# 
# #tidy by terms in topics
# lda_1_tidy <- tidy(lda_1, matrix = "beta") 
# 
# #plot most popular words in topics
# lda_1_tidy %>%
#   group_by(topic) %>%
#   arrange(desc(beta)) %>%
#   top_n(10) %>%
#   ggplot(aes(x = beta, y = term)) +
#   geom_col() +
#   facet_wrap(~topic, scales = "free_y")


# using stm and Silge's workflow:
library(stm)
plan(multisession)

posts_sparse <- threads_tokenized |>
  count(id, word) |> 
  filter(n > 3) |> 
  cast_sparse(id, word, n)


many_models <- data_frame(K = c(5, 10, 15, 20)) %>%
  mutate(topic_model = future_map(K, ~stm(posts_sparse, K = .,
                                          verbose = FALSE), seed = TRUE))




heldout <- make.heldout(posts_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, posts_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, posts_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result


k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "15 topics seems to work best?")

#soo, held out likelihood is biggest at 15 topics, residuals are pretto low there as well
#semantic coherence is worse tho

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(5, 10, 15, 20)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

#so 15 topics seems like a decent choice?


## lets look at the results of our topic model with 15 topics
topic_model <- k_result %>% 
  filter(K == 15) %>% 
  pull(topic_model) %>% 
  .[[1]]

summary(topic_model)


td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  arrange(desc(beta)) %>%
  ggplot(aes(x = beta, y = fct_reorder(term, beta), fill = as.factor(topic))) +
  geom_col() +
  facet_wrap(~topic, scales = "free",nrow = 3) +
  theme(legend.position = "none")


#we can start to make out some topics. E.g, Topics 2 and 4 are probably about job. Topic 9 is definitely school, topic 7 is about renting while 5 is about home.

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(posts_sparse))

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols= c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.20),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = "gamma",
       title = "Top topics by prevalence in the revenge stories",
       subtitle = "With the top words that contribute to each topic")
# so topic 2 about jobs is the most common one and then house topic and school



#### Sentiment analysis ----

#do I want to vader thois?
library(vader)

# we can get sentiment scores that follow basic syntactic rules using vader
#' package. It will give us score for positive, negative and net sentiment for
#' each revenge story
#' Fair warning - vader takes a while

vader_sent <- vader::vader_df(threads$text)



# emotion words with nrc

#' antoher approach would be to use dictionary methods and use some dictionary
#' for the analysis. An example would be the nrc lexicon
#' We need to get the nrc lexicon and then we can count it with threads_tokenized
library(tidytext)
# get nrc lexicon
nrc <- tidytext::get_sentiments(lexicon = "nrc")

nrc_stories <- threads_tokenized %>%
  inner_join(nrc, by = "word")

#count emotion words
nrc_stories <- nrc_stories %>%
  group_by(id) %>%
  count(sentiment, name = "n_words")

#get summaries

nrc_stories %>%
  group_by(id) %>%
  mutate(percent = n_words/sum(n_words)) %>%
  group_by(sentiment) %>%
  summarise(mean = mean(percent))



nrc_stories %>%
  group_by(id) %>%
  mutate(percent = n_words/sum(n_words)) %>%
  ggplot(aes(x = sentiment, y = percent, color = sentiment)) +
  geom_jitter()

#join with tokenized
#count



#' There are lots of other interesting things we might be interested in.
#' For example revenge stories usually have some structure to them - 
#' They start by describing some wrongdoing (i.e. the motivation for revenge)
#' followed by the act of revenge and sometimes the aftermath. This means we
#' can expect some dynamics of emotionality in the text from negative to more
#' positive (or mixed if revenge is truly bitter sweet). We could try to analyze
#' this by looking at emotionality e.g. per paragraph or sentence rather than in
#' entire stories and then looking at how sentiment changes in the stories.


#### topics and engagement ----
#ok, so now lets try to do some modelling. We could try to e.g. predict
# the score of the story bases on its topic, emotionality etc. Basically,
# can we find a recipe for a good revenge story?
to_filter <- threads_tokenized |>
  count(id, word) |> 
  filter(n > 3)

for_model <- threads %>%
  mutate(id = 1:940) %>%
  semi_join(to_filter, by = "id")


set.seed(123)

effects <-
  estimateEffect(
    1:15 ~ score,
    topic_model,
    for_model 
  )

tidy(effects) |> 
  filter(term != "(Intercept)", p.value < 0.05)



#try to pivot wider to get 1 variable per topic proportion and use them to predict

gamma_wide <- td_gamma %>%
  mutate(topic = paste0("topic_", topic)) %>%
  pivot_wider(id_cols = "document", names_from = "topic", values_from = "gamma")

gamma_score <- gamma_wide %>%
  inner_join(threads %>% mutate(id = as.character(1:940)), by = c("document" = "id"))


model1 <- lm(score ~ comments, gamma_score %>% select(score, topic_1:topic_15, comments))
summary(model1)
#ok, does not really work...
