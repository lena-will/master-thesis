# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(igraph)
library(wordcloud)
library(RColorBrewer)

# exploring the most common words in the topics --------------------------------

load("/Users/lena/Documents/R/master_thesis/final/lda_60.Rda")
beta <- tidy(lda_final, matrix = "beta")

topics <- beta %>% 
  group_by(topic) %>% 
  arrange(desc(beta), .by_group = TRUE) %>% 
  slice_max(beta, n = 50) %>% 
  ungroup()

#write.csv(topics, "/Users/lena/Documents/R/master_thesis/topics_60.csv")

# word clouds:

# Health Care
wordcloud_topic_health <- topics %>% 
  filter(topic == 38)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_topic_health$term, freq = wordcloud_topic_health$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# Finanzkrise
wordcloud_topic_finanzkrise <- topics %>% 
  filter(topic == 15)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_topic_finanzkrise$term, freq = wordcloud_topic_finanzkrise$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# Energiewende
wordcloud_topic_energie <- topics %>% 
  filter(topic == 17)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_topic_energie$term, freq = wordcloud_topic_energie$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# Öffentlicher Haushalt
wordcloud_topic_haushalt <- topics %>% 
  filter(topic == 34)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_topic_haushalt$term, freq = wordcloud_topic_haushalt$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# Negative Sentiment
wordcloud_negative_sentiment <- topics %>% 
  filter(topic == 12)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_negative_sentiment$term, freq = wordcloud_negative_sentiment$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# Elections
wordcloud_elections <- topics %>% 
  filter(topic == 18)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_elections$term, freq = wordcloud_elections$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

# network graphs:
network_topics <- beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 17) %>% 
  ungroup()

test_topic <- network_topics %>% 
  filter(topic == 2)
topic2_string <- test_topic$term
topic2_network <- paste0(topic2_string, collapse = "-")
network_test <- graph_from_literal(soldat-bundeswehr-afghanistan-deutsch-werden-einsatz-sein-jahr-müssen-mehr-militärisch-schon-truppe-neu-geben-erst-amerikanisch)
plot(network_test)





