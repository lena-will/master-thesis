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

load("/Users/lena/Documents/R/master_thesis/final/attention_issue_final.Rda")

# monthly aggregation ----------------------------------------------------------

attention_month <- attention_issue %>% 
  mutate(month = lubridate::floor_date(Date, 'month'), .after = Date) %>% 
  group_by(month) %>% 
  summarise(across(-(Date), mean))





