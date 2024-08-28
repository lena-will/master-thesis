# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(stringi)
library(tidyr)
library(quanteda)
library(tm)
library(topicmodels)

# Load data and LDA results ----------------------------------------------------

load(file = "/Users/lena/Documents/R/master_thesis/final/lda_60.Rda")

lda_terms <- lda_final@terms

folder_list <- list.files("/Users/lena/Desktop/faz_data/utf_8")
path <- "/Users/lena/Desktop/faz_data/utf_8"
file_name <- "artikel_df.csv"
artikel_df = NULL

for (f in 1:length(folder_list)) {
  folder_name <- file.path(folder_list[f])
  data_tmp <- read.csv(file.path(path, folder_name, file_name)) %>%
    rename(artikel_id = X)
  artikel_df <- artikel_df %>%
    rbind(data_tmp)
}

rm(f, file_name, folder_name, data_tmp)

# collapse articles from the same issue into one large document ----------------

artikel_collapsed <- artikel_df %>% 
  select(c(Datum, Stemming)) %>% 
  group_by(Datum) %>% 
  summarise(Issue = paste(Stemming, collapse = ""))

# compute doc-term matrix for new corpus ---------------------------------------

issue_corpus <- quanteda::corpus(artikel_collapsed$Issue)
dtm_issue <- DocumentTermMatrix(issue_corpus)

dtm_issue <- dtm_issue[,dtm_issue$dimnames$Terms %in% lda_terms]

# compute topic distribution for collapsed articles (for an issue) -------------

control_issue = list(
  seed = 1234,
  estimate.beta = FALSE,
  alpha = 50/60,
  delta = 0.01,
  iter = 2000,
  burnin = 500,
  thin = 10,
  best = TRUE
)

issue_lda <- LDA(dtm_issue, model = lda_final, control = control_issue)

issue_gamma <- tidy(issue_lda, matrix = "gamma")

issue_gamma_wide <- issue_gamma %>% 
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  cbind(artikel_collapsed$Datum) %>% 
  rename(Date = `artikel_collapsed$Datum`) %>% 
  relocate(Date, .after = document)

save(issue_gamma_wide, file = "/Users/lena/Documents/R/master_thesis/final/issue_collapsed.Rda")  
write.csv(issue_gamma_wide, file = "/Users/lena/Documents/R/master_thesis/final/issue_collapsed.csv")
