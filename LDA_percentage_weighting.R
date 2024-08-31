# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
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
dtm <- DocumentTermMatrix(lda_corpus,
                          control = list(
                            stopwords = FALSE,
                            removePunctuation = FALSE
                          ))

dtm_weighting_prep <- tapply((dtm$v != 0), dtm$j, length)/nDocs(dtm)
dtm_weighting <- dtm[, dtm_weighting_prep >= 0.001]
dtm_weighting <- dtm_weighting[slam::row_sums(dtm_weighting) > 0, ]

# define parameters ------------------------------------------------------------

k = 60

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

# LDA --------------------------------------------------------------------------

lda <- LDA(dtm_weighting,
           k = k,
           method = "Gibbs",
           control = control_lda)
beta <- tidy(lda, matrix = "beta")

topics <- beta %>% 
  group_by(topic) %>% 
  arrange(desc(beta), .by_group = TRUE) %>%
  slice_max(beta, n = 50)

save.image("/Users/lena/...")
#write.csv(topics, "/Users/lena/...")
