library(tidyverse)
library(tidytext)

data("stop_words")

read_rds("./data/covid_19.rds") -> df

custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("english"),
                                      lexicon = "custom"))

unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

df %>%
  select(text) %>%
  mutate(text = stringi::stri_trans_general(text, "Latin-ASCII"),
         text = str_to_lower(text),
         text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  # mutate(word = SnowballC:: wordStem(word)) %>%
  filter(!word %in% custom_stop_words$word) %>%
  count(word, sort = T) %>%
  mutate(type = case_when(str_detect(word, "^@") ~ "mention",
                          str_detect(word, "^#") ~ "hashtag",
                          TRUE ~ "text"),
         type = as_factor(type)) -> df_b


df_b %>%
  filter(!word %in% c("#coronavirus", "#covid19", "rt",
                      "#covid_19", "#"),
         type == "mention") %>%
  top_n(20, n) %>%
  ggplot(aes(fct_reorder(word, n), n, fill = n)) +
  geom_col(colour="black", width=0.5) +
  ggtitle("Frequencia de palabras en Tweet Corpus") +
  xlab(NULL) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill=F, color=F) +
  coord_flip() +
  theme_minimal(base_size = 20)


