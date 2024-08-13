# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(quanteda)
library(tm)
library(topicmodels)

# Load data and LDA results ----------------------------------------------------

load(file = "/Users/lena/Documents/R/master_thesis/LDA environments/beta_03.Rda")
load(file = "/Users/lena/Documents/R/master_thesis/LDA environments/gamma_03.Rda")

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

# compute topic distribution for collapsed articles (for an issue) -------------

control_issue = list(
  seed = 1234,
  estimate.beta = FALSE,
  alpha = 50/80,
  iter = 2000,
  burnin = 500,
  thin = 10,
  best = TRUE
)

issue_lda <- LDA(dtm_issue, model = lda, control = control_issue)
#save(issue_lda, file = "/Users/lena/Documents/R/master_thesis/issue_lda_03.Rda")

# get article-topic mapping ----------------------------------------------------

text_id <- unique(gamma$document)
match_id <- artikel_df %>% 
  select(c(artikel_id, Datum)) %>% 
  cbind(as.data.frame(text_id)) %>% 
  rename(document = text_id)

artikel_info <- merge(gamma, match_id, by = "document") %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))

article_topic_mapping <- NULL

topics <- sort(unique(artikel_info$topic))

for (t in 1:length(topics)) {
  mapping_tmp <- artikel_info %>% 
    group_by(Datum) %>% 
    filter(topic == t) %>% 
    mutate(mapping = max(gamma)) %>% 
    ungroup() %>% 
    filter(gamma == mapping)
  
  article_topic_mapping <- article_topic_mapping %>% 
    rbind(mapping_tmp)
}

# get positiv and negativ German words -----------------------------------------

positiv_de <- read.csv("/Users/lena/Documents/R/master_thesis/tone/positiv_de.csv") %>% 
  mutate(positiv = tolower(positiv)) %>% 
  distinct(positiv)
negativ_de <- read.csv("/Users/lena/Documents/R/master_thesis/tone/negativ_de.csv") %>% 
  mutate(negativ = tolower(negativ)) %>% 
  distinct(negativ)


  
