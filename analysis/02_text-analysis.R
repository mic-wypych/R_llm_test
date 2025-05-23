#' this script contains the llm based analysis of the texts
library(ellmer)
library(mall)
library(data.table)
library(dplyr)

llm_use("ollama", "llama3.2:latest", seed = 100, temperature = 0)

#### threads -------
## basic threads analysis
threads <- read.csv("data/threads.csv")

# test basic llm functions on a subset of the data
threads_head5 <- threads[1:5,]

threads_head5 |>
  llm_sentiment(text, options = c("sad", "angry","disappointed", "disgusted", "happy", "satisfied")) |>
  select(text, .sentiment)
#well this is sloooooooow. Might want to invest in some gpt?

## cleaning text?
# get to lower
# get rid of all the \n \r etc?


# analysis:
#' get some general sentiment?
#' get some description of change in sentiment or mix of emotions?
#' get descriptions of topics


#### comments -----------
#' how do I match comments to threads?
#' IDEA 1: detect 1 as the first comment for each thread and then fill downwards?
#' how do we then number the join var so that it will be from 1 to 907?

comments <- fread("data/comments.csv")
comments <- as.data.frame(comments)


com_head5 <- comments[1:5,]
com_head5 |>
  llm_sentiment(comment, options = c("sad", "angry","disappointed", "disgusted")) |>
  select(comment, .sentiment)
