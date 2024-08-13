# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(tm)
library(topicmodels)
library(slam)

# import data from python analysis ---------------------------------------------

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

# create weighted corpus -------------------------------------------------------

lda_corpus <- quanteda::corpus(artikel_df$Stemming)
dtm <- DocumentTermMatrix(lda_corpus)

term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) * log10(nDocs(dtm)/slam::col_sums(dtm > 0))
dtm_weighting <- dtm[, term_tfidf >= 0.011] # adjust dtm for weighting
dtm_weighting <- dtm_weighting[slam::row_sums(dtm_weighting) > 0,] # remove those articles which don't have any relevant tokens after adjusting

# define parameters ------------------------------------------------------------

k = 100

control_lda <- list(
  alpha = 50 / k,
  delta = 0.01,
  estimate.beta = TRUE,
  verbose = 0,
  seed = 1234,
  iter = 10000,
  burnin = 2000,
  thin = 100,
  best = TRUE
)

lda <- LDA(dtm,
           k = k,
           method = "Gibbs",
           control = control_lda)

#save.image("/Users/lena/...")
