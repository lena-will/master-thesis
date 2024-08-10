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

# work with subset of full dataset for developing:
# artikel_subset <- artikel_df %>%
#   slice(1:30)
# artikel_holdout <- artikel_df %>% 
#   slice(31:51)
# complete_artikel <- artikel_subset %>% 
#   rbind(artikel_holdout)
complete_artikel <- artikel_df

load(file = "/Users/lena/Documents/R/master_thesis/beta_library_lda.Rda")
load(file = "/Users/lena/Documents/R/master_thesis/gamma_library_lda.Rda")

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

time_series_topic <- artikel_info %>% 
  group_by(topic, month = lubridate::floor_date(Datum, 'month')) %>% 
  mutate(gamma_topic_month = sum(gamma)) %>% 
  select(c(topic, month, gamma_topic_month)) %>% 
  distinct()

time_series_sum<- artikel_info %>% 
  group_by(month = lubridate::floor_date(Datum, 'month')) %>% 
  mutate(gamma_sum = sum(gamma)) %>% 
  select(c(topic, month, gamma_sum)) %>% 
  distinct()
  
time_series <- time_series_topic %>% 
  left_join(time_series_sum) %>% 
  mutate(attention = gamma_topic_month/gamma_sum)

monetary_economics <- time_series %>% 
  filter(topic == 46)

plot_topic_attention <- monetary_economics %>% 
  ggplot(aes(x = month, y = attention, group = 1)) +
  geom_line() +
  scale_x_date(name = "Date", date_breaks = "1 year", date_labels="%Y") +
  labs(x = "Date", y = "Monetary Policy") +
  theme_classic()
plot(plot_topic_attention)


# save for further analysis
save(time_series, file = "/Users/lena/Documents/R/master_thesis/attention.Rda")

