library(tidytext)
sentiments
get_sentiments("bing")
library(janeaustenr)
library(stringr)
library(tidytext)
tidy_data<-austen_books()%>%
  grouping(book)%>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^chapter
                                              [\\divxlc]",
                                              ignore_case = TRUE))))%>%
  ungroup()%>%
  unnest_tokens(word,text)

positive_senti<-get_sentiments("bing")%>%
  filter(sentiment=="positive")
tidy_data%>%
  filter(book=="Emma")%>%
  semi_join(positive_senti)%>%
  count(word(,sort=TRUE))
library(tidyr)
bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(ggplot2)
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
