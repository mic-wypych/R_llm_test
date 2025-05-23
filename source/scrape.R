library(RedditExtractoR)
library(readr)

#get thread ursl
revenge_urls <- find_thread_urls(subreddit="ProRevenge", period='all')

saveRDS(revenge_urls, "revenge_urls.RDS")

#get all the comments. The API seems to be fairly limiting now so lets try to parse it in chunks
results <- list()
i_1 <- 702
i_2 <- 752
for(i in 1:5) {
  revenge_comments <- RedditExtractoR::get_thread_content(revenge_urls[i_1:i_2,]$url)
  results <- append(results, list(revenge_comments))
  i_1 <- i_1 + 50
  i_2 <- i_2 + 50
  Sys.sleep(180)
}

saveRDS(results, "threads_comments_1-900.RDS")


results <- readRDS("threads_comments_1-550.RDS")
revenge_urls <- readRDS("revenge_urls.RDS")

#ok we got to 350 posts with their comments

#wrangling the list

threads_df <- results[[1]][[1]]
comments_df <- results[[1]][[2]]

for (i in 2:length(results)) {
  tmp_threads <- results[[i]][[1]]
  tmp_comments <- results [[i]][[2]]

  threads_df <- rbind(threads_df, tmp_threads)
  comments_df <- rbind(comments_df, tmp_comments)
}

test_df <- lapply(results, \(x) rbind(x[[1]]))


#### saving files
write.csv(file = "data/threads_all.csv", revenge_urls)

write.csv(file = "data/comments.csv", x = comments_df)

write.csv(file = "data/threads.csv", x = threads_df)
