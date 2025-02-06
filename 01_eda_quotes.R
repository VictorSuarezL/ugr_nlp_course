library(tidyverse)
library(tidytext)

data("stop_words")

read_csv("./data/insparation.csv") -> df

df %>%
  glimpse()

df %>%
  select(id = "...1", Category, Quote) %>%
  mutate(id = as.character(id)) %>%
  unnest_tokens(word, Quote) %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(str_detect(word, "[:alpha:]")) -> df

df %>%
  count(Category, sort = T)

df %>%
  count(word, sort = T) %>%
  arrange(n) %>%
  filter(str_detect(word, "[:alpha:]")) -> df

df %>%
  count(Category, word) %>%
  group_by(Category) %>%
  mutate(proportion = n/sum(n)) %>%
  ungroup() %>%
  arrange(desc(proportion))

df %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Category) %>%
  summarise(total_sent = sum(value),
            sd_sent = sd(value)) %>%
  ungroup() %>%
  arrange(sd_sent)
  # arrange(desc(sd_sent))
  # arrange(total_sent)
  # arrange(desc(total_sent))













