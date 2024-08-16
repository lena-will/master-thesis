# Housekeeping -----------------------------------------------------------------

library(tidytext)
library(tidyverse)
library(quanteda)
library(lubridate)

# load data --------------------------------------------------------------------

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

complete_artikel <- artikel_df

#load(file = "/Users/lena/Documents/R/master_thesis/beta_library_lda.Rda")
#load(file = "/Users/lena/Documents/R/master_thesis/gamma_library_lda.Rda")
load(file = "/Users/lena/Documents/R/master_thesis/LDA environments/lda_05.Rda")

lda <- lda_final

beta <- tidy(lda, matrix = "beta")
gamma <- tidy(lda, matrix = "gamma")

# document distributions over topics -------------------------------------------

text_id <- unique(gamma$document)
match_id <- complete_artikel %>% 
  select(c(artikel_id, Datum)) %>% 
  cbind(as.data.frame(text_id)) %>% 
  rename(document = text_id)

artikel_info <- merge(gamma, match_id, by = "document") %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))

# monthly aggregation

time_series_topic <- artikel_info %>% 
  group_by(topic, month = lubridate::floor_date(Datum, 'month')) %>% 
  mutate(gamma_topic_month = sum(gamma)) %>% 
  select(c(topic, month, gamma_topic_month)) %>% 
  distinct() %>% 
  ungroup()

time_series_sum<- artikel_info %>% 
  group_by(month = lubridate::floor_date(Datum, 'month')) %>% 
  mutate(gamma_sum = sum(gamma)) %>% 
  select(c(topic, month, gamma_sum)) %>% 
  distinct() %>% 
  ungroup()
  
time_series_month <- time_series_topic %>% 
  left_join(time_series_sum) %>% 
  mutate(attention = gamma_topic_month/gamma_sum)

# issue aggregation

time_series_topic_issue <- artikel_info %>% 
  group_by(topic, Datum) %>% 
  mutate(gamma_topic_issue = sum(gamma)) %>% 
  select(c(topic, Datum, gamma_topic_issue)) %>% 
  distinct() %>% 
  ungroup()

time_series_sum_issue<- artikel_info %>% 
  group_by(Datum) %>% 
  mutate(gamma_sum_issue = sum(gamma)) %>% 
  select(c(topic, Datum, gamma_sum_issue)) %>% 
  distinct() %>% 
  ungroup()

time_series_issue <- time_series_topic_issue %>% 
  left_join(time_series_sum_issue) %>% 
  mutate(attention_issue = gamma_topic_issue/gamma_sum_issue)

# format

attention_issue <- time_series_issue %>% 
  select(c(topic, Datum, attention_issue)) %>% 
  pivot_wider(names_from = topic, values_from = attention_issue) %>% 
  rename(Date = Datum)

attention_month <- time_series_month %>% 
  select(c(topic, month, attention)) %>% 
  pivot_wider(names_from = topic, values_from = attention) %>% 
  rename(Date = month)

# save for further analysis ----------------------------------------------------
save(attention_issue, file = "/Users/lena/Documents/R/master_thesis/attention_issue.Rda")
save(attention_month, file = "/Users/lena/Documents/R/master_thesis/attention_month.Rda")

# use as input to python script to map publication date to quarter week


