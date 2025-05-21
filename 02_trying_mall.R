ollamar::pull("gemma3:4b")

ollamar::test_connection()
ollamar::list_models()


#data
library(readr)

threads <- read_csv("data/threads_all.csv")
comments <- 
#working with mall

library(mall)

llm_use("ollama", "gemma3:4b")

#some basic sentiment

sentiment <- threads |>
  llm_sentiment(text)

#summaries?


#classify to topics?

threads |>
  llm_classify(text, c("work", "relationships"))


#some custom topic thingy?

my_prompt <- paste(
  "Answer a question.",
  "Return only the answer, no explanation",
  "Acceptable answers are 'yes', 'no'",
  "Answer this about the following text, is this a text funny?:"
)

threads |>
  llm_custom(text, my_prompt)