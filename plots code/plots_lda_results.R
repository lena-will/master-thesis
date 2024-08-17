# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(igraph)
library(wordcloud)
library(RColorBrewer)

# exploring the most common words in the topics --------------------------------

load("/Users/lena/Documents/R/master_thesis/lda_035.Rda")
beta <- tidy(lda_final, matrix = "beta")

topics <- beta %>% 
  group_by(topic) %>% 
  arrange(desc(beta), .by_group = TRUE) %>% 
  slice_max(beta, n = 50) %>% 
  ungroup()

write.csv(topics, "/Users/lena/Documents/R/master_thesis/topics_035.csv")

# word clouds:

wordcloud_topic2 <- topics %>% 
  filter(topic == 46)
# for coloured word clouds
pal <- brewer.pal(9,"Set1")
wordcloud(wordcloud_topic2$term, freq = wordcloud_topic2$beta, random.order = TRUE, colors = pal, scale=c(1.5,.1))

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





