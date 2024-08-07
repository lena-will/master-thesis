# housekeeping -----------------------------------------------------------------

library(tidyverse)

# load perplexity scores from lda analysis -------------------------------------

load("/Users/lena/Documents/R/master_thesis/perplexity.Rda")

# plot perplexity scores -------------------------------------------------------

plot_perplexity <- check_perplexity %>% 
  ggplot(aes(x = topic_options)) +
  geom_line(aes(y = perplexity_scores)) +
  geom_point(aes(y = perplexity_scores)) +
  xlab("Number of topics") +
  ylab("Perplexity score")
plot_perplexity
  
