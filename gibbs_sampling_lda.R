# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(tm)
library(topicmodels)

# import data from python analysis ---------------------------------------------

folder_list <- list.files("/Users/lena/...")
path <- "/Users/lena/..."
file_name <- "artikel_df.csv"
artikel_df = NULL

for (f in 1:length(folder_list)) {
  folder_name <- file.path(folder_list[f])
  data_tmp <- read.csv(file.path(path, folder_name, file_name)) %>%
    rename(artikel_id = X)
  artikel_df <- artikel_df %>%
    rbind(data_tmp)
}

# work with subset of full dataset for developing:
artikel_subset <- artikel_df %>%
  slice(1:10)
artikel_holdout <- artikel_df %>% 
  slice(11:14)
#artikel_subset <- artikel_df

# create document term-matrix --------------------------------------------------
artikel_id <- artikel_subset$artikel_id
distinct_terms_ini <- str_c(artikel_subset$Stemming, sep = " ") # initialise vocabulary
distinct_terms <- paste(distinct_terms_ini, collapse = " ")
vocabulary_terms <- strsplit(distinct_terms, " ")[[1]]

vocabulary <- sort(unique(vocabulary_terms)) # get unique terms which constitute the vocabulary, sorting is crucial!
relevant_artikel <- str_split(artikel_subset$Stemming, " ") # artikel
occurences <- lapply(relevant_artikel, table)

dtm <- matrix(
  data = 0L,
  # initialise doc-term-matrix
  ncol = length(vocabulary),
  nrow = length(relevant_artikel),
  dimnames = list(artikel_id, vocabulary)
)

for (ii in 1:length(occurences)) {
  # fill doc-term-matrix
  term <- occurences[[ii]]
  dtm[ii, names(term)] <- as.integer(term)
}

# assign term IDs to terms in vocabulary

term_id_key <- as.data.frame(vocabulary) %>%
  mutate(term_id = 1:length(vocabulary))

artikel_numercial <- NULL
for (ii in 1:length(relevant_artikel)) {
  artikel_numercial[[ii]] <- match(relevant_artikel[[ii]], vocabulary)
}

# --------------------- Gibbs sampling for LDA ---------------------------------

# Initialisation ---------------------------------------------------------------

# input: dtm, alpha and eta hyperparameters, number of topics
K = 5
alpha <- 50 / K
eta <- 200 / length(vocabulary)

# initialise matrices storing the counts and topic assignments:
nmk <- matrix(data = 0L,
              nrow = length(relevant_artikel),
              ncol = K) # nmk is a M x K  matrix which stores how many words in a document are assigned to each topic
nkt <- matrix(data = 0L,
              nrow = K,
              ncol = length(vocabulary)) # mkt is a K x V matrix which stores the number of times a term t is assigned to topic k
zmn <- lapply(artikel_numercial, function(x)
  rep(0, length(x)))
store_phi <- list() # list to store parameter matrices (phi)
store_theta <- list() # list to store parameter matrices (gamma)

# initialise topics by randomly assigning each word a topic and adjusting counts:

set.seed(1234)

for (m in 1:length(artikel_numercial)) {
  for (n in 1:length(artikel_numercial[[m]])) {
    zmn[[m]][n] = sample(K, 1) # randomly sample topic for word
    topic_tmp = zmn[[m]][n] # get topic
    term_id_tmp = artikel_numercial[[m]][n] # get term_id
    nkt[topic_tmp, term_id_tmp] = nkt[topic_tmp, term_id_tmp] + 1 # increase count in nkt count matrix
  }
  for (k in 1:K) {
    nmk[m, k] = sum(zmn[[m]] == k) # count how many words in doc m are assigned to each topic
  }
}


# Sampling ---------------------------------------------------------------------

# burn-in period:

burn_in = 500 #define number of burn-in iterations


for (ii in 1:burn_in) {
  for (m in 1:length(artikel_numercial)) {
    for (n in 1:length(artikel_numercial[[m]])) {
      # decrementing counts
      prev_word_topic = zmn[[m]][n]
      term_id_tmp = artikel_numercial[[m]][n]
      nkt[prev_word_topic, term_id_tmp] = nkt[prev_word_topic, term_id_tmp] - 1 # decrement topic-term count, i.e. exclude term i from topic
      nmk[m, prev_word_topic] = nmk[m, prev_word_topic] - 1 # decrement document-topic count, i.e. exclude term i from the document
      # sample from full conditional
      full_conditional <- ((nkt[, term_id_tmp] + eta) / (rowSums(nkt) + eta)) * (nmk[m, ] + alpha)
      zmn[[m]][n] = sample(K, 1, prob = full_conditional)
      # incrementing counts
      new_word_topic <- zmn[[m]][n]
      nkt[new_word_topic, term_id_tmp] = nkt[new_word_topic, term_id_tmp] + 1
      nmk[m, new_word_topic] = nmk[m, new_word_topic] + 1
    }
  }
}

#sampling

nmk_sampling <- matrix(data = 0L,
              nrow = length(relevant_artikel),
              ncol = K) # nmk is a M x K  matrix which stores how many words in a document are assigned to each topic
nkt_sampling <- matrix(data = 0L,
              nrow = K,
              ncol = length(vocabulary)) # mkt is a K x V matrix which stores the number of times a term t is assigned to topic k

for (m in 1:length(artikel_numercial)) {
  for (n in 1:length(artikel_numercial[[m]])) {
    zmn[[m]][n] = zmn[[m]][n] # get previous topic assignment
    topic_tmp = zmn[[m]][n] # get topic
    term_id_tmp = artikel_numercial[[m]][n] # get term_id
    nkt_sampling[topic_tmp, term_id_tmp] = nkt_sampling[topic_tmp, term_id_tmp] + 1 # increase count in nkt count matrix
  }
  for (k in 1:K) {
    nmk_sampling[m, k] = sum(zmn[[m]] == k) # count how many words in doc m are assigned to each topic
  }
}

# go into sampling

sampling_iter = 2000

for (ii in 1:sampling_iter) {
  for (m in 1:length(artikel_numercial)) {
    for (n in 1:length(artikel_numercial[[m]])) {
      # decrementing counts
      prev_word_topic = zmn[[m]][n]
      term_id_tmp = artikel_numercial[[m]][n]
      nkt_sampling[prev_word_topic, term_id_tmp] = nkt_sampling[prev_word_topic, term_id_tmp] - 1 # decrement topic-term count, i.e. exclude term i from topic
      nmk_sampling[m, prev_word_topic] = nmk_sampling[m, prev_word_topic] - 1 # decrement document-topic count, i.e. exclude term i from the document
      # sample from full conditional
      full_conditional <- ((nkt_sampling[, term_id_tmp] + eta) / (rowSums(nkt_sampling) + eta)) * (nmk_sampling[m, ] + alpha)
      zmn[[m]][n] = sample(K, 1, prob = full_conditional)
      # incrementing counts
      new_word_topic <- zmn[[m]][n]
      nkt_sampling[new_word_topic, term_id_tmp] = nkt_sampling[new_word_topic, term_id_tmp] + 1
      nmk_sampling[m, new_word_topic] = nmk_sampling[m, new_word_topic] + 1
    }
  }
  if (ii %% 10 == 0) { # store samples every 10th draw
    store_phi[[ii]] <- (nkt_sampling + eta) / (rowSums(nkt_sampling) + eta)
    store_theta[[ii]] <- (nmk_sampling + alpha) / (rowSums(nmk_sampling) + alpha)
  } else next
}


# read out parameters (average of the last 10 stored draws):

store_phi[sapply(store_phi, is.null)] <- NULL
store_theta[sapply(store_theta, is.null)] <- NULL

phi_10 <- store_phi[-c(1:190)]
phi_average <- Reduce("+", phi_10)/length(phi_10)

theta_10 <- store_theta[-c(1:190)]
theta_average <- Reduce("+", theta_10)/length(theta_10)

# saving for post processing
#save(phi_average, file = "/Users/lena/...")
#save(theta_average, file = "/Users/lena/...")

# format parameter matrices:

# probabilities of term t being assigned to topic k
phi_df <- as.data.frame(phi_average)
colnames(phi_df) <- vocabulary
phi_df <- phi_df %>%
  mutate(topic = seq(1, K, 1))

phi_df_long <- phi_df %>%
  pivot_longer(!topic, values_to = "phi")

# article-topic attention
theta_df <- as.data.frame(theta_average)
colnames(theta_df) <- seq(1, K, 1)
theta_df <- theta_df %>%
  mutate(artikel_id = artikel_subset$artikel_id)
theta_df_long <- theta_df %>%
  pivot_longer(!artikel_id, names_to = "topic", values_to = "theta")

# some first analysis ----------------------------------------------------------

topic_words <- phi_df_long %>%
  group_by(topic) %>%
  arrange(desc(phi), .by_group = TRUE) %>%
  slice_max(phi, n = 10)
