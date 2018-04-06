library(tidytext)
library(dplyr)
library(readtext)
library(ggplot2)
library(gridExtra)




eisenhower <- read.delim("eisenhower first state of the union.txt", header=FALSE)

#We create a paragraph for each line detected by read.delim and we tidy up the speech

eisenhower1 <- tibble(paragraph = 1:165, text = as.character(eisenhower$V1)) %>% 
  unnest_tokens(word, text)

#We count the recurrence of the same words in the same paragraph and we weigh them
#according to whether they have a positive or a negative influence

eis1 <- eisenhower1 %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word,index = paragraph %/% 2,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


plot1 <- ggplot(eis1, aes(index, sentiment) ) +
  geom_col(show.legend = FALSE) + labs(x = "Sentiment bars in Eisenhower's speech ")

eis_words_count <- eisenhower1 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

plot2 <- eis_words_count %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment in Eisenhower's speech",
       x = NULL) +
  coord_flip()





nixon <- read.delim("nixon first state of the union.txt", header=FALSE)

#We create a paragraph for each line detected by read.delim and we tidy up the speech

nixon1 <- tibble(paragraph = 1:132, text = as.character(nixon$V1)) %>% 
  unnest_tokens(word, text)

#We count the recurrence of the same words in the same paragraph and we weigh them
#according to whether they have a positive or a negative influence

nix1 <- nixon1 %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word,index = paragraph %/% 2,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


plot3 <- ggplot(nix1, aes(index, sentiment) ) +
  geom_col(show.legend = FALSE) + labs(x = "Sentiment bars in Nixon's speech ")

nix_words_count <- nixon1 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

plot4 <- nix_words_count %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment in Nixon's speech",
       x = NULL) +
  coord_flip()

#----DRAFT ANALYSIS----


#First Comparison
grid.arrange(plot1, plot3)

#We can see that according to the way we decided to weigh the words
#Eisenhower's speech turned out to be much more engaging and dramatic
#as we can see peaks reaching values such as 15 and 10

#In Nixon's speech we see that the bars rarely exceed 4 and -4, resulting in a much more
#contained speech.


#Second Comparison

grid.arrange(plot2,plot4)


#By this second comparison, we can definitely see that both the speeches
#are built up on words that belong to a similar semantic context.

#however in Eisenhower's speech we can recognize words such as 'defensive'
#'attack' and 'tension' that reflect the historic period he was living in,
#trying to manage tensions under the looming threat of nuclear weapons
#with the Soviet Union. As a matter of fact, we don't find these words
#in Nixon's speech.

#Analyzing the positive words, we can see
#that the first used words are about freedom and respect.
#Contrary to Nixon, Eisenhower favore a more moderate course of Republicanism,
#which preserved individual freedom and the marekt economy.

