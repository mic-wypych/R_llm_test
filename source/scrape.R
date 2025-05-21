library(RedditExtractoR)
library(readr)

#get thread ursl
revenge_urls <- find_thread_urls(subreddit="ProRevenge", period='all')



#get all the comments. The API seems to be fairly limiting now so lets try to parse it in chunks
results <- list()
i_1 <- 1
i_2 <- 50
for(i in 1:18) {
  revenge_comments <- RedditExtractoR::get_thread_content(revenge_urls[i:i_2,]$url)
  results <- append(results, list(revenge_comments))
  i_1 <- i_1 + 50
  i_2 <- i_2 + 50
  Sys.sleep(300)
}


#wrangling the list


#### saving files
write_csv(file = "data/threads_all.csv", revenge_urls)

write_csv(file = "data/comments.csv", x = revenge_comments$comments)

write_csv(file = "data/threads.csv", x = revenge_comments$threads)
