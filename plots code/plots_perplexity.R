# housekeeping -----------------------------------------------------------------

library(tidyverse)

# load perplexity scores from lda analysis -------------------------------------

load("/Users/lena/Documents/R/master_thesis/LDA environments/perplexity_02.Rda")

check_perplexity$topic_options <- paste("$K$ = ", check_perplexity$topic_options, sep=" ")

perplexity_wide <- check_perplexity %>% 
  pivot_wider(names_from = topic_options, values_from = perplexity_scores)

kable(perplexity_wide, "latex")

# plot perplexity scores -------------------------------------------------------

plot_perplexity <- check_perplexity %>% 
  ggplot(aes(x = topic_options)) +
  geom_line(aes(y = perplexity_scores)) +
  geom_point(aes(y = perplexity_scores)) +
  xlab("Number of topics") +
  ylab("Perplexity score")
plot_perplexity
  
