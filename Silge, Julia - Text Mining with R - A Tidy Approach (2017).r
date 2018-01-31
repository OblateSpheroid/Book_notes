### Silge Book Notes ###
# Chapter 1

require(tidytext)
library(dplyr)

text <- c("Because I could not stop for Death -",
"He kindly stopped for me -",
"The Carriage held but just Ourselves -",
"and Immortality")

text2df <- function(t) data_frame(line = 1:length(t), text = t)

text_df <- text2df(text)
unnest_tokens(text_df, word, text)

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
data(stop_words)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- unnest_tokens(original_books, word, text) %>%
  anti_join(stop_words)

count(tidy_books, word, sort = TRUE)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(gutenbergr)
library(scales)
library(tidyr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_hgwells <-  unnest_tokens(hgwells, word, text) %>%
  anti_join(stop_words)

tidy_bronte <-  unnest_tokens(bronte, word, text) %>%
  anti_join(stop_words)

count(tidy_hgwells, word, sort = TRUE)
count(tidy_bronte, word, sort = TRUE)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Chapter 2

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <-  filter(get_sentiments("nrc"), sentiment == "joy")
filter(tidy_books, book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

janeaustensentiment <-  inner_join(tidy_books, get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")