# predicting score and engagement with topics and sentiment and shit

td_gamma_wide <- td_gamma %>%
  pivot_wider(id_cols = document, names_from = topic, values_from = gamma)

threads$id <- as.character(1:940)


threads_topics <- threads %>%
  left_join(td_gamma_wide, by = c("id" = "document"))

topics_form <- 

fit.1 <- lm(score ~ `1` + `2` + `3` + `4` + `5` + `6` + `7` 
            + `8` + `9` + `10` + `11` + `12` + `13` + `14` + `15` + timestamp + comments,
   threads_topics)

summary(fit.1)

#ok so far this is total crap


nrc_wide_stories <- nrc_stories %>%
  pivot_wider(id_cols = id, names_from = sentiment, values_from = n_words) %>%
  mutate(across(everything(), \(x) replace_na(x, 0)),
         id = as.character(id)) 

sent_form <- as.formula(paste(unique(nrc_stories$sentiment), collapse = " + "))

threads_topics_nrc <- threads_topics %>%
left_join(nrc_wide_stories, by = "id")

lm.2 <- lm(score ~ anger + anticipation + disgust + fear + joy + negative + positive + sadness + surprise + trust, threads_topics_nrc)
summary(lm.2)

#well poop this also sucks