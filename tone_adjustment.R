# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(stringi)
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

#issue_lda <- LDA(dtm_issue, model = lda, control = control_issue)
#save(issue_lda, file = "/Users/lena/Documents/R/master_thesis/issue_lda_03.Rda")
load(file = "/Users/lena/Documents/R/master_thesis/issue_lda_03.Rda")

gamma_issue <- as.data.frame(issue_lda@gamma) %>% 
  cbind(artikel_collapsed$Datum) %>% 
  rename(Datum = `artikel_collapsed$Datum`) %>% 
  relocate(Datum, .before = 1)

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

check <- article_topic_mapping %>% 
  group_by(Datum) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n != 80)

filter_date <- article_topic_mapping %>% 
  filter(Datum == "2002-03-17")

artikel_df_prep <- artikel_df %>% 
  select(c(artikel_id, Stemming))
article_topic_mapping_text <- merge(article_topic_mapping, artikel_df_prep, by = "artikel_id")

# get positive and negative German words -----------------------------------------

positiv_de <- readr::read_delim("/Users/lena/Documents/R/master_thesis/GermanPolarityClues-Positive-Lemma-21042012.tsv", col_names = FALSE) 
negativ_de <- readr::read_delim("/Users/lena/Documents/R/master_thesis/GermanPolarityClues-Negative-Lemma-21042012.tsv", col_names = FALSE) 
colnames_tsv <- c("Term", "Lemma", "PoS", "Sentiment", "Score", "D")
colnames(positiv_de) <- colnames_tsv
colnames(negativ_de) <- colnames_tsv

positiv_de <- positiv_de %>% 
  select(Term) %>% 
  mutate(Term = tolower(Term)) %>% 
  distinct(Term)

negativ_de <- negativ_de %>% 
  select(Term) %>% 
  mutate(Term = tolower(Term)) %>% 
  distinct(Term)

common_terms <- intersect(positiv_de$Term, negativ_de$Term)  

positiv_de <- positiv_de %>% 
  filter(! Term %in% common_terms)

negativ_de <- negativ_de %>% 
  filter(! Term %in% common_terms)

# count number of positive and negative words in article mapping ---------------

positiv_vector <- as.vector(positiv_de$Term)
negativ_vector <- as.vector(negativ_de$Term)
tmp_corpus <- quanteda::corpus(article_topic_mapping_text$Stemming)
tmp_dtm <- DocumentTermMatrix(tmp_corpus)  

counts_positiv <- tmp_dtm[, tmp_dtm$dimnames$Terms %in% positiv_vector]
counts_negativ <- tmp_dtm[, tmp_dtm$dimnames$Terms %in% negativ_vector]

sum_positiv <- as.data.frame(slam::row_sums(counts_positiv)) %>% 
  rename(positiv_counts = 1)

sum_negativ <- as.data.frame(slam::row_sums(counts_negativ)) %>% 
  rename(negativ_counts = 1)

# get number of terms per article ----------------------------------------------

terms_per_article <- as.data.frame(slam::row_sums(tmp_dtm)) %>% 
  rename(total_terms = 1)

# Get normalised tone counts ---------------------------------------------------

tone_counts <- article_topic_mapping_text %>% 
  cbind(sum_positiv) %>% 
  cbind(sum_negativ) %>% 
  cbind(terms_per_article)

tone_counts_normalised <- tone_counts %>% 
  mutate(positiv_counts = positiv_counts/total_terms) %>% 
  mutate(negativ_counts = negativ_counts/total_terms)

# tone adjustment of topic frequencies per issue -------------------------------

tone_adjustment <- tone_counts_normalised %>% 
  mutate(tone_adjustment = positiv_counts - negativ_counts)


