library(RedditExtractoR)
library(readr)

#get thread ursl
revenge_urls <- find_thread_urls(subreddit="ProRevenge", period='all')

#get all the comments
revenge_comments <- RedditExtractoR::get_thread_content(revenge_urls$url)

#save all threads, recovered threads and comments to csvs
write_csv(file = "data/threads_all.csv", revenge_urls)

write_csv(file = "data/comments.csv", x = revenge_comments$comments)

write_csv(file = "data/threads.csv", x = revenge_comments$threads)
