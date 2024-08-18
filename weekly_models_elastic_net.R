# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(glmnet)

# load text-based indicators ---------------------------------------------------
attention <- read.csv("/Users/lena/Documents/R/master_thesis/attention_issue_mapping.csv") %>%
  select(-X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

col_names <- c("Date", "quarter_avail", "week_avail", rep(1:60, 1))
colnames(attention) <- col_names

attention$month <- as.Date(sub("\\d{2}$", "1", attention$Date))

attention <- attention %>% 
  relocate(month, .before = quarter_avail)

# attention <- attention %>%
#   relocate(quarter_avail, .after = Date) %>%
#   relocate(week_avail, .after = quarter_avail)

# load macro data --------------------------------------------------------------

# esi

esi <- read.csv("/Users/lena/Documents/R/master_thesis/data/esi.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# cpi

cpi <- read.csv("/Users/lena/Documents/R/master_thesis/data/cpi.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# vacancies 

vacancies <- read.csv("/Users/lena/Documents/R/master_thesis/data/vacancies.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# term spread

term_spread <- read.csv("/Users/lena/Documents/R/master_thesis/data/term_spread.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 7,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 11,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0))

# ip index 

ip_index <- read.csv("/Users/lena/Documents/R/master_thesis/data/ip_index.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 11,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 0,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0))

# gdp 

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv") %>% 
  rename(week = quarter_week)

#Period 1: Recession - trainings sample: 2005Q1-2007Q2

min_train <- "2001-10-01"
min_test <- "2007-07-01"
max_test <- "2009-06-01"

# Model 1

source("/Users/lena/Git/master-thesis/functions/elasticNet_w1.R")

week1 <- elasticNet_w1(gdp, attention, min_train, min_test, max_test)

# Model 2








