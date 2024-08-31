# Housekeeping -----------------------------------------------------------------

library(tidyverse)

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
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
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
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
  relocate(week, .after = Date) %>% 
  select(-CPI)

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
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
  relocate(week, .after = Date) %>% 
  select(-vacancies)

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
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
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
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
  relocate(IP, .after = last_col())


# gdp 

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv") %>% 
  rename(week = quarter_week) %>% 
  filter(Date >= "2002-01-01" & Date < "2024-04-01")

# ------------------------------ Nowcasting ------------------------------------

source("/Users/lena/Git/master-thesis/functions/baseline_models/baseline_w4.R")
source("/Users/lena/Git/master-thesis/functions/baseline_models/baseline_w7.R")
source("/Users/lena/Git/master-thesis/functions/baseline_models/baseline_w9.R")
source("/Users/lena/Git/master-thesis/functions/baseline_models/baseline_w11.R")
source("/Users/lena/Git/master-thesis/functions/baseline_models/baseline_w13.R")

# Period 1: Recession ----------------------------------------------------------

min_train_p1 <- "2002-01-01"
max_train_p1 <- "2007-04-01"
max_test_p1 <- "2009-04-01"

baseline_w4_p1 <- baseline_w4(gdp, esi_bridge, cpi_bridge, vacancies_bridge, min_train_p1, max_train_p1, max_test_p1)
baseline_w7_p1 <- baseline_w7(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p1, max_train_p1, max_test_p1)
baseline_w9_p1 <- baseline_w9(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p1, max_train_p1, max_test_p1)
baseline_w11_p1 <- baseline_w11(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p1, max_train_p1, max_test_p1)
baseline_w13_p1 <- baseline_w13(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p1, max_train_p1, max_test_p1)

results_baseline_p1 <- c(baseline_w4_p1, baseline_w7_p1, baseline_w9_p1, baseline_w11_p1, baseline_w13_p1)
results_baseline_p1 <- as.data.frame(results_baseline_p1)

rm(baseline_w4_p1, baseline_w7_p1, baseline_w9_p1, baseline_w11_p1, baseline_w13_p1)

# Period 2: Cyclical Stability -------------------------------------------------

min_train_p2 <- "2002-01-01"
max_train_p2 <- "2014-04-01"
max_test_p2 <- "2016-04-01"

baseline_w4_p2 <- baseline_w4(gdp, esi_bridge, cpi_bridge, vacancies_bridge, min_train_p2, max_train_p2, max_test_p2)
baseline_w7_p2 <- baseline_w7(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p2, max_train_p2, max_test_p2)
baseline_w9_p2 <- baseline_w9(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p2, max_train_p2, max_test_p2)
baseline_w11_p2 <- baseline_w11(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p2, max_train_p2, max_test_p2)
baseline_w13_p2 <- baseline_w13(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p2, max_train_p2, max_test_p2)

results_baseline_p2 <- c(baseline_w4_p2, baseline_w7_p2, baseline_w9_p2, baseline_w11_p2, baseline_w13_p2)
results_baseline_p2 <- as.data.frame(results_baseline_p2)

rm(baseline_w4_p2, baseline_w7_p2, baseline_w9_p2, baseline_w11_p2, baseline_w13_p2)

# Period 3: COVID pandemic -----------------------------------------------------

min_train_p3 <- "2002-01-01"
max_train_p3 <- "2019-04-01"
max_test_p3 <- "2021-04-01"

baseline_w4_p3 <- baseline_w4(gdp, esi_bridge, cpi_bridge, vacancies_bridge, min_train_p3, max_train_p3, max_test_p3)
baseline_w7_p3 <- baseline_w7(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p3, max_train_p3, max_test_p3)
baseline_w9_p3 <- baseline_w9(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p3, max_train_p3, max_test_p3)
baseline_w11_p3 <- baseline_w11(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p3, max_train_p3, max_test_p3)
baseline_w13_p3 <- baseline_w13(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p3, max_train_p3, max_test_p3)

results_baseline_p3 <- c(baseline_w4_p3, baseline_w7_p3, baseline_w9_p3, baseline_w11_p3, baseline_w13_p3)
results_baseline_p3 <- as.data.frame(results_baseline_p3)

rm(baseline_w4_p3, baseline_w7_p3, baseline_w9_p3, baseline_w11_p3, baseline_w13_p3)


# Period 4: Now ----------------------------------------------------------------

min_train_p4 <- "2002-01-01"
max_train_p4 <- "2022-04-01"
max_test_p4 <- "2024-04-01"

baseline_w4_p4 <- baseline_w4(gdp, esi_bridge, cpi_bridge, vacancies_bridge, min_train_p4, max_train_p4, max_test_p4)
baseline_w7_p4 <- baseline_w7(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p4, max_train_p4, max_test_p4)
baseline_w9_p4 <- baseline_w9(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, min_train_p4, max_train_p4, max_test_p4)
baseline_w11_p4 <- baseline_w11(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p4, max_train_p4, max_test_p4)
baseline_w13_p4 <- baseline_w13(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train_p4, max_train_p4, max_test_p4)

results_baseline_p4 <- c(baseline_w4_p4, baseline_w7_p4, baseline_w9_p4, baseline_w11_p4, baseline_w13_p4)
results_baseline_p4 <- as.data.frame(results_baseline_p4)

rm(baseline_w4_p4, baseline_w7_p4, baseline_w9_p4, baseline_w11_p4, baseline_w13_p4)
