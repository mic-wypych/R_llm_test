ollamar::pull("llama3.2")

ollamar::test_connection()
ollamar::list_models()


#data
library(readr)

threads <- read_csv("data/threads_all.csv")
comments <- 
#working with mall

library(mall)

llm_use("ollama", "llama3.2:latest", seed = 100, temperature = 0)

#some basic sentiment

sentiment <- head_5 |>
  llm_sentiment(text) |>
  select(.sentiment)

#summaries?


#classify to topics?

head_5 |>
  llm_classify(text, c("work", "relationships")) |>
  select(.classify)


#some custom topic thingy?

my_prompt <- paste(
  "Below is a representative set of stories about revenge.",
  "Please identify the single main topic mentioned in these stories Return a topic name",
  "The topic name should be short, but descriptive e.g. `work` or `family`. They are all about revenge so do not assign topic revenge",
  "If you cannot find a good topic label, just say, return `none`",
  "Return only the answer, no explanation"
)



head_5 <- threads %>%
  slice_head(n = 5)

#get number of words
sapply(gregexpr("[[:alpha:]]+", head_5$text), function(x) sum(x > 0)) |> sum()

head_5 |>
  llm_custom(text, my_prompt, pred_name = "topic") |>
  select(text, topic)


llm_vec_custom("I got revenge on my boss because he stole my car. I spilled cocoa on his head and sprinkled it with confetti right before his important speech", my_prompt)


#ok for now this is going ridiculously slow
#is there any way to actually speed this up? evaluating 5 texts takes looong. Nowhere near the 1.5 minutes per 20k words in the mall article...