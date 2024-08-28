# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(kableExtra)

install.packages("kableExtra")# Load LDA results for word distributions --------------------------------------

load("/Users/lena/Documents/R/master_thesis/final/lda_60.Rda")
beta <- tidy(lda_final, matrix = "beta")

topics <- beta %>% 
  group_by(topic) %>% 
  arrange(desc(beta), .by_group = TRUE) %>% 
  slice_max(beta, n = 25) %>% 
  ungroup()

topics_table_ini <- topics %>% 
  select(-beta) %>% 
  group_by(topic) %>% 
  mutate(first_terms = paste0(term, collapse = ", ")) %>% 
  ungroup %>% 
  select(-term) %>% 
  distinct()

kable(topics_table_ini, "latex")
