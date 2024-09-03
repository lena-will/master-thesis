# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(tm)
library(topicmodels)

# Load keyword inflation -------------------------------------------------------

folder_list <- list.files("/Users/lena/Desktop/faz_data/utf_8")
path <- "/Users/lena/Desktop/faz_data/utf_8"
file_name <- "keyword_unemp.csv"
keyword_unemp = NULL

for (f in 1:length(folder_list)) {
  folder_name <- file.path(folder_list[f])
  data_tmp <- read.csv(file.path(path, folder_name, file_name)) %>%
    rename(artikel_id = X)
  keyword_unemp <- keyword_unemp %>%
    rbind(data_tmp)
}

# Load recession dates for plotting --------------------------------------------

recessions <- read.csv("/Users/lena/Documents/R/master_thesis/recessions_germany.csv")

recessions <- recessions %>% 
  mutate(Peak = as.Date(Peak, format = "%Y-%m-%d")) %>% 
  mutate(Trough = as.Date(Trough, format = "%Y-%m-%d"))

recessions[1,1] <- "2001-09-01" # alter peak for visualisation purposes

# collapse articles from the same issue into one large document ----------------

keyword_collapsed <- keyword_unemp %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>% 
  mutate(month = lubridate::floor_date(Datum, 'month'), .after = Datum) %>% 
  select(c(Datum, key_sentences_string, month)) %>% 
  group_by(month) %>% 
  summarise(Issue = paste(key_sentences_string, collapse = ""))

corpus_inflation <- quanteda::corpus(keyword_collapsed$Issue)
dtm_inflation <- DocumentTermMatrix(corpus_inflation)

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

counts_positiv <- dtm_inflation[, dtm_inflation$dimnames$Terms %in% positiv_vector]
counts_negativ <- dtm_inflation[, dtm_inflation$dimnames$Terms %in% negativ_vector]

sum_positiv <- as.data.frame(slam::row_sums(counts_positiv)) %>% 
  rename(positiv_counts = 1)

sum_negativ <- as.data.frame(slam::row_sums(counts_negativ)) %>% 
  rename(negativ_counts = 1)

# get number of terms per article ----------------------------------------------

terms_per_article <- as.data.frame(slam::row_sums(dtm_inflation)) %>% 
  rename(total_terms = 1)

# Get normalised tone counts ---------------------------------------------------

tone_counts <- keyword_collapsed %>% 
  cbind(sum_positiv) %>% 
  cbind(sum_negativ) %>% 
  cbind(terms_per_article)

tone_counts_normalised <- tone_counts %>% 
  mutate(positiv_counts = positiv_counts/total_terms) %>% 
  mutate(negativ_counts = negativ_counts/total_terms) %>% 
  mutate(tone_adjustment = positiv_counts - negativ_counts)

# Standardise time-series ------------------------------------------------------

time_series_z <- tone_counts_normalised %>% 
  select(month, tone_adjustment) %>% 
  mutate(tone_adjustment = scale(tone_adjustment, center = TRUE, scale = TRUE))

# plot keyword_inflation -------------------------------------------------------

plot_timeseries <- ggplot(time_series_z) +
  geom_line(aes(x = month, y = tone_adjustment, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Unemployment") +
  theme_classic() +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(plot_timeseries)
