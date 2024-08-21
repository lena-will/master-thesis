# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tseries)

# ------------------ Augmeted Dickey-Fuller Tests ------------------------------

# macro data

esi <- read.csv("/Users/lena/Documents/R/master_thesis/data/esi.csv") %>% 
  select(-c(X, Date)) %>% 
  filter(row_number() <= n()-1) %>% 
  slice(-1)

cpi <- read.csv("/Users/lena/Documents/R/master_thesis/data/cpi.csv") %>% 
  select(-c(X, Date, CPI)) %>% 
  filter(row_number() <= n()-1) %>% 
  slice(-1)

vacancies <- read.csv("/Users/lena/Documents/R/master_thesis/data/vacancies.csv") %>% 
  select(-c(X, Date, vacancies)) %>% 
  filter(row_number() <= n()-1) %>% 
  slice(-1)

term_spread <- read.csv("/Users/lena/Documents/R/master_thesis/data/term_spread.csv") %>% 
  select(-c(X, availability, Date)) %>% 
  filter(row_number() <= n()-1) %>% 
  slice(-1)

ip_index <- read.csv("/Users/lena/Documents/R/master_thesis/data/ip_index.csv") %>% 
  select(-c(X, availability, Date, IP)) %>% 
  slice(-1)

time_series <- esi %>% 
  cbind(cpi) %>% 
  cbind(vacancies) %>% 
  cbind(term_spread) %>% 
  cbind(ip_index)

columns <- c("series", "p_value")
test_results <- data.frame(matrix(nrow = ncol(time_series), ncol = length(columns))) 
colnames(test_results) <- columns


for (ii in 1:ncol(time_series)){
  adf_test <- adf.test(time_series[,ii])
  test_results$series[ii] = colnames(time_series)[ii]
  test_results$p_value[ii] = adf_test$p.value
}

# monthly attention ------------------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/attention_month.Rda")
attention_series <- attention_month %>% 
  select(-Date)

attention_series <- as.matrix(attention_series)

attention_results <- data.frame(matrix(nrow = ncol(attention_series), ncol = length(columns))) 
colnames(attention_results) <- columns

for (ii in 1:ncol(attention_series)){
  print(ii)
  adf_test <- adf.test(attention_series[,ii])
  attention_results$series[ii] = colnames(attention_series)[ii]
  attention_results$p_value[ii] = adf_test$p.value
}

# compute mom growth rates


attention_mom_growth <- attention_month %>%
  mutate(across(all_of(c(2:ncol(attention_month))), ~ ., .names = "{col}_growth")) %>% 
  select(-Date)

attention_mom_growth_num <- mutate_all(attention_mom_growth, function(x) as.numeric(as.character(x)))

skip <- ncol(attention_month) - 1

for (j in 1:(ncol(attention_month)-1)) {
  for (ii in 2:nrow(attention_mom_growth_num)) {
    attention_mom_growth_num[ii,j + skip] = ((attention_mom_growth_num[ii,j] - attention_mom_growth_num[ii-1,j])/attention_mom_growth_num[ii-1,j])*100
  }
}

attention_mom <- attention_mom_growth_num %>% 
  select(ends_with("growth"))
attention_mom <- as.matrix(attention_mom)

attention_results_mom <- data.frame(matrix(nrow = ncol(attention_mom), ncol = length(columns))) 
colnames(attention_results_mom) <- columns

for (ii in 1:ncol(attention_mom)){
  adf_test <- adf.test(attention_mom[,ii])
  attention_results_mom$series[ii] = colnames(attention_mom)[ii]
  attention_results_mom$p_value[ii] = adf_test$p.value
}

# weekly attention -------------------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/attention_issue.Rda")
attention_weekly <- attention_issue %>% 
  select(-Date)

attention_weekly <- as.matrix(attention_weekly)

attention_results_weekly <- data.frame(matrix(nrow = ncol(attention_weekly), ncol = length(columns))) 
colnames(attention_results_weekly) <- columns

for (ii in 1:ncol(attention_weekly)){
  adf_test <- adf.test(attention_weekly[,ii])
  attention_results_weekly$series[ii] = colnames(attention_weekly)[ii]
  attention_results_weekly$p_value[ii] = adf_test$p.value
}

# compute growth rates




