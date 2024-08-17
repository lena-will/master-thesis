# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(lubridate)

z_scores <- function(x) {
  (x - mean(x)) / sd(x)
}

# load text-based indicators ---------------------------------------------------

attention <- read.csv("/Users/lena/Documents/R/master_thesis/attention_issue_mapping.csv") %>%
  select(-X)

col_names <- c("Date", rep(1:60, 1), "quarter_avail", "week_avail")
colnames(attention) <- col_names

attention <- attention %>%
  relocate(quarter_avail, .after = Date) %>%
  relocate(week_avail, .after = quarter_avail)

# esi --------------------------------------------------------------------------

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

# cpi --------------------------------------------------------------------------

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

# vacancies --------------------------------------------------------------------

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

# term spread ------------------------------------------------------------------

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

# ip index ---------------------------------------------------------------------

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



