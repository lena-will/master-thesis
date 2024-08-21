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
  relocate(month, .before = quarter_avail) %>% 
  slice(-1) %>% 
  filter(Date < "2024-04-01")

# attention <- attention %>%
#   relocate(quarter_avail, .after = Date) %>%
#   relocate(week_avail, .after = quarter_avail)

source("/Users/lena/Git/master-thesis/functions/bridge_data_average.R")

start_col <- 5
attention_bridge <- bridge_data_average(attention, start_col)

attention_bridge <- attention_bridge %>%
  select(c(Date, month, quarter_avail, week_avail, ends_with("_b")))

# load macro data  -------------------------------------------------------------

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
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13)) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  relocate(week, .after = Date)

esi_bridge <- esi %>%
  mutate(across(all_of(c(
    3:ncol(esi)
  )), ~ ., .names = "{col}_b"))

skip_esi <- ncol(esi) - 2
for (ii in 1:nrow(esi_bridge)) {
  if (esi_bridge$week[ii] == 4) {
    esi_bridge[ii, 3 + skip_esi] = esi_bridge[ii, 3]
  } else if (esi_bridge$week[ii] == 9) {
    esi_bridge[ii, 3 + skip_esi] = (esi_bridge[ii, 3] + esi_bridge[ii - 1, 3]) / 2
  } else {
    esi_bridge[ii, 3 + skip_esi] = (esi_bridge[ii, 3] + esi_bridge[ii - 1, 3] + esi_bridge[ii - 2, 3]) / 3
  }
}

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
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13)) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  relocate(week, .after = Date)

cpi_bridge <- cpi %>%
  mutate(across(all_of(c(
    3:ncol(cpi)
  )), ~ ., .names = "{col}_b"))

skip_cpi <- ncol(cpi) - 2
for (ii in 1:nrow(cpi_bridge)) {
  if (cpi_bridge$week[ii] == 4) {
    cpi_bridge[ii, 3 + skip_cpi] = cpi_bridge[ii, 3]
  } else if (cpi_bridge$week[ii] == 9) {
    cpi_bridge[ii, 3 + skip_cpi] = (cpi_bridge[ii, 3] + cpi_bridge[ii - 1, 3]) / 2
  } else {
    cpi_bridge[ii, 3 + skip_cpi] = (cpi_bridge[ii, 3] + cpi_bridge[ii - 1, 3] + cpi_bridge[ii - 2, 3]) / 3
  }
}


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
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13)) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  relocate(week, .after = Date)

vacancies_bridge <- vacancies %>%
  mutate(across(all_of(c(
    3:ncol(vacancies)
  )), ~ ., .names = "{col}_b"))

skip_vacancies <- ncol(vacancies) - 2
for (ii in 1:nrow(vacancies_bridge)) {
  if (vacancies_bridge$week[ii] == 4) {
    vacancies_bridge[ii, 3 + skip_vacancies] = vacancies_bridge[ii, 3]
  } else if (vacancies_bridge$week[ii] == 9) {
    vacancies_bridge[ii, 3 + skip_vacancies] = (vacancies_bridge[ii, 3] + vacancies_bridge[ii - 1, 3]) / 2
  } else {
    vacancies_bridge[ii, 3 + skip_vacancies] = (vacancies_bridge[ii, 3] + vacancies_bridge[ii - 1, 3] + vacancies_bridge[ii - 2, 3]) / 3
  }
}

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
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0)) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  relocate(spread, .after = last_col())

term_spread_bridge <- term_spread %>%
  mutate(across(all_of(c(
    4:ncol(term_spread)
  )), ~ ., .names = "{col}_b"))

skip_term_spread <- ncol(term_spread) - 3
for (ii in 1:nrow(term_spread_bridge)) {
  if (term_spread_bridge$week[ii] == 7) {
    term_spread_bridge[ii, 4 + skip_term_spread] = term_spread_bridge[ii, 4]
  } else if (term_spread_bridge$week[ii] == 11) {
    term_spread_bridge[ii, 4 + skip_term_spread] = (term_spread_bridge[ii, 4] + term_spread_bridge[ii - 1, 4]) / 2
  } else {
    term_spread_bridge[ii, 4 + skip_term_spread] = term_spread_bridge[ii, 4]
  }
}

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
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0)) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  relocate(IP, .after = last_col())


# gdp 

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv") %>% 
  rename(week = quarter_week) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01")

# load weekly models

source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w1.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w2.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w3.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w4.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w5.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w6.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w7.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w8.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w9.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w10.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w11.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w12.R")
source("/Users/lena/Git/master-thesis/functions/elasticNet_models/elasticNet_w13.R")


#Period 1: Recession - training sample: 2001Q4-2007Q2 --------------------------

min_train <- "2001-10-01"
min_test <- "2007-07-01"
max_test <- "2009-07-01"

# Model 1

week1_p1 <- elasticNet_w1(gdp, attention_bridge, min_train, min_test, max_test)

# Model 2

week2_p1 <- elasticNet_w2(gdp, attention_bridge, min_train, min_test, max_test)

# Model 3

week3_p1 <- elasticNet_w3(gdp, attention_bridge, min_train, min_test, max_test)

# Model 4

week4_p1 <- elasticNet_w4(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 5

week5_p1 <- elasticNet_w5(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 6

week6_p1 <- elasticNet_w6(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 7

week7_p1 <- elasticNet_w7(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 8

week8_p1 <- elasticNet_w8(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 9

week9_p1 <- elasticNet_w9(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 10

week10_p1 <- elasticNet_w10(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 11

week11_p1 <- elasticNet_w11(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 12

week12_p1 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 13

week13_p1 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

#Period 2: Cyclical Stability - training sample: 2001Q4-2014Q2 -----------------

min_train <- "2001-10-01"
min_test <- "2014-07-01"
max_test <- "2016-07-01"

# Model 1

week1_p2 <- elasticNet_w1(gdp, attention_bridge, min_train, min_test, max_test)

# Model 2

week2_p2 <- elasticNet_w2(gdp, attention_bridge, min_train, min_test, max_test)

# Model 3

week3_p2 <- elasticNet_w3(gdp, attention_bridge, min_train, min_test, max_test)

# Model 4

week4_p2 <- elasticNet_w4(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 5

week5_p2 <- elasticNet_w5(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 6

week6_p2 <- elasticNet_w6(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 7

week7_p2 <- elasticNet_w7(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 8

week8_p2 <- elasticNet_w8(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 9

week9_p2 <- elasticNet_w9(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 10

week10_p2 <- elasticNet_w10(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 11

week11_p2 <- elasticNet_w11(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 12

week12_p2 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 13

week13_p2 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

#Period 3: Covid - training sample: 2001Q4-2019Q2 ------------------------------

min_train <- "2001-10-01"
min_test <- "2019-07-01"
max_test <- "2021-07-01"

# Model 1

week1_p3 <- elasticNet_w1(gdp, attention_bridge, min_train, min_test, max_test)

# Model 2

week2_p3 <- elasticNet_w2(gdp, attention_bridge, min_train, min_test, max_test)

# Model 3

week3_p3 <- elasticNet_w3(gdp, attention_bridge, min_train, min_test, max_test)

# Model 4

week4_p3 <- elasticNet_w4(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 5

week5_p3 <- elasticNet_w5(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 6

week6_p3 <- elasticNet_w6(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 7

week7_p3 <- elasticNet_w7(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 8

week8_p3 <- elasticNet_w8(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 9

week9_p3 <- elasticNet_w9(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 10

week10_p3 <- elasticNet_w10(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 11

week11_p3 <- elasticNet_w11(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 12

week12_p3 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 13

week13_p3 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

#Period 4: Now - training sample: 2001Q4-2014Q2 --------------------------------

min_train <- "2001-10-01"
min_test <- "2022-07-01"
max_test <- "2024-04-01"

# Model 1

week1_p4 <- elasticNet_w1(gdp, attention_bridge, min_train, min_test, max_test)

# Model 2

week2_p4 <- elasticNet_w2(gdp, attention_bridge, min_train, min_test, max_test)

# Model 3

week3_p4 <- elasticNet_w3(gdp, attention_bridge, min_train, min_test, max_test)

# Model 4

week4_p4 <- elasticNet_w4(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 5

week5_p4 <- elasticNet_w5(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 6

week6_p4 <- elasticNet_w6(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, min_train, min_test, max_test)

# Model 7

week7_p4 <- elasticNet_w7(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 8

week8_p4 <- elasticNet_w8(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 9

week9_p4 <- elasticNet_w9(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 10

week10_p4 <- elasticNet_w10(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, min_train, min_test, max_test)

# Model 11

week11_p4 <- elasticNet_w11(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 12

week12_p4 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)

# Model 13

week13_p4 <- elasticNet_w12(gdp, attention_bridge, esi_bridge, vacancies_bridge, cpi_bridge, term_spread_bridge, ip_index, min_train, min_test, max_test)




