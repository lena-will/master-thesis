# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(stringi)
library(tidyr)
library(tidytext)

# Load data and LDA results ----------------------------------------------------

load(file = "/Users/lena/Documents/R/master_thesis/final/lda_60.Rda")
gamma <- tidy(lda_final, matrix = "gamma")

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

# load text-based indicators ---------------------------------------------------

load(file = "/Users/lena/Documents/R/master_thesis/final/issue_collapsed.Rda")
gamma_issue <- issue_gamma_wide %>% 
  pivot_longer(cols = -c(document, Date)) %>% 
  rename(topic = name) %>% 
  rename(gamma = value)

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
  filter(n != 60)

dates_check <- as.Date(check$Datum)

revised_mapping <- NULL

for (ii in 1:length(dates_check)){
  article_topic_mapping_check <- article_topic_mapping %>%
    filter(Datum == dates_check[ii]) %>%
    group_by(topic) %>%
    filter(length(topic) == 2) %>%
    slice(-1)
  revised_mapping <- revised_mapping %>%
    rbind(article_topic_mapping_check)
}

article_topic_mapping_decrease <- article_topic_mapping %>% 
  anti_join(revised_mapping)

artikel_df_prep <- artikel_df %>% 
  select(c(artikel_id, Stemming))
article_topic_mapping_text <- merge(article_topic_mapping_decrease, artikel_df_prep, by = "artikel_id")

# get positive and negative German words -----------------------------------------

positiv_de <- readr::read_delim("/Users/lena/Documents/R/master_thesis/tone/GermanPolarityClues-Positive-Lemma-21042012.tsv", col_names = FALSE) 
negativ_de <- readr::read_delim("/Users/lena/Documents/R/master_thesis/tone/GermanPolarityClues-Negative-Lemma-21042012.tsv", col_names = FALSE) 
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
  mutate(tone_adjustment = positiv_counts - negativ_counts) %>% 
  select(c(topic, Datum, tone_adjustment)) %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>% 
  mutate(topic = as.numeric(topic))

gamma_issue <- gamma_issue %>% 
  #rename(Datum = Date) %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>% 
  mutate(topic = as.numeric(topic))

tone_adjusted_attention <- gamma_issue %>% 
  left_join(tone_adjustment) %>% 
  mutate(tone_adjusted_gamma = gamma*tone_adjustment)

tone_adjusted_attention_wide <- tone_adjusted_attention %>% 
  select(-c(document, gamma, tone_adjustment)) %>% 
  pivot_wider(names_from = topic, values_from = tone_adjusted_gamma)

save(tone_adjusted_attention_wide, file = "/Users/lena/Documents/R/master_thesis/tone_adjusted_attention.Rda")
write.csv(tone_adjusted_attention_wide, file = "/Users/lena/Documents/R/master_thesis/tone_adjusted_attention.csv")
