# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(glmnet)
library(zoo)
library(lubridate)

# load data --------------------------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/attention_issue.Rda")

text_data_long <- time_series_issue %>% 
  select(c(Datum, topic, attention_issue)) %>% 
  arrange(topic)

text_data <- text_data_long %>% 
  pivot_wider(names_from = topic, values_from = attention_issue)

rm(text_data_long, time_series_issue)

macro_data <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# aggregate text-indicators to quarterly frequency -----------------------------

test_indicator <- text_data %>% 
  select(Datum, `1`) %>% 
  arrange(Datum)

test_indicator_mean <- test_indicator %>%
  group_by(Month = format(as.yearqtr(Datum, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>% 
  select(Month, `1`)
