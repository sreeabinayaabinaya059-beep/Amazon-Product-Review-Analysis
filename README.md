install.packages(c("tidyverse", "tidytext", "textdata", "wordcloud", "RColorBrewer"))


library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(RColorBrewer)


reviews <- tibble::tibble(
  review_id = 1:8,
  review_text = c(
    "The product quality is amazing, I love it!",
    "Terrible experience, waste of money.",
    "The item was okay, not too bad but not great.",
    "Absolutely fantastic! Works perfectly.",
    "Worst product ever, very disappointed.",
    "Decent product for the price. Could be better.",
    "Excellent quality, fast delivery, very satisfied!",
    "The product stopped working after two days."
  )
)


tidy_reviews <- reviews %>%
  unnest_tokens(word, review_text)


data("stop_words")
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words, by = "word")


bing_sentiments <- get_sentiments("bing")

sentiment_data <- tidy_reviews %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(review_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

final_reviews <- reviews %>%
  left_join(sentiment_data, by = "review_id") %>%
  mutate(
    sentiment_label = case_when(
      sentiment_score > 0 ~ "Positive",
      sentiment_score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

print(final_reviews)


library(ggplot2)

ggplot(final_reviews, aes(x = sentiment_label, fill = sentiment_label)) +
  geom_bar() +
  labs(
    title = "Amazon Product Review Sentiment Distribution",
    x = "Sentiment",
    y = "Count"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


all_words <- tidy_reviews %>%
  count(word, sort = TRUE)

set.seed(123)
wordcloud(
  words = all_words$word,
  freq = all_words$n,
  min.freq = 1,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE
)
