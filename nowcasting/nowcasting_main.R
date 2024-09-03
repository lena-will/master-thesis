# Housekeeping -----------------------------------------------------------------

library(tidyverse)

# load text-based indicators ---------------------------------------------------

attention_ini <- read.csv("/Users/lena/Documents/R/master_thesis/final/issue_collapsed.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

attention_ini$month <- as.Date(sub("\\d{2}$", "1", attention_ini$Date))

attention <- attention_ini %>% 
  relocate(month, .after = Date) %>% 
  filter(Date >= "2002-01-01" & Date < "2024-04-01") %>% 
  select(-X)

source("/Users/lena/Git/master-thesis/functions/bridge_data_average.R")

start_col <- 5
attention_bridge <- bridge_data_average(attention, start_col)

# attention_bridge <- attention_bridge %>%
#   select(c(Date, month, quarter_avail, week_avail, ends_with("_b")))

attention_bridge <- attention_bridge %>%
  select(c(Date, month, week_avail, ends_with("_b")))

# adjust the two quarters which does not have week 13 ()
attention_bridge_week12_07 <- attention_bridge %>% 
  filter(Date == "2007-03-25")

attention_bridge <- attention_bridge %>% 
  rbind(attention_bridge_week12_07) %>% 
  arrange(Date)

index_week12_07 <- which(attention_bridge$Date == "2007-03-25")
attention_bridge[index_week12_07[2], 3] = 13

attention_bridge_week12_18 <- attention_bridge %>% 
  filter(Date == "2018-03-25")

attention_bridge <- attention_bridge %>% 
  rbind(attention_bridge_week12_18) %>% 
  arrange(Date)

index_week12_18 <- which(attention_bridge$Date == "2018-03-25")
attention_bridge[index_week12_18[2], 3] = 13


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
      month(Date) == 10 ~ 10,
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

load("/Users/lena/Documents/R/master_thesis/preselection_results005.Rda")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w1.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w2.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w3.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w4.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w5.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w6.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w7.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w8.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w9.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w10.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w11.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w12.R")
source("/Users/lena/Git/master-thesis/functions/weekly_models/model_w13.R")

# Period 1: Recession ----------------------------------------------------------

min_train_p1 <- "2002-01-01"
max_train_p1 <- "2007-04-01"
max_test_p1 <- "2009-04-01"

p1_preselection <- preselection_results$p1
p1_preselection$keyword <- paste0(p1_preselection$keyword, "_b")
p1_preselection <- as.vector(p1_preselection[1])[[1]]

model_w1_p1 <- model_w1(gdp, attention_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w2_p1 <- model_w2(gdp, attention_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w3_p1 <- model_w3(gdp, attention_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w4_p1 <- model_w4(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w5_p1 <- model_w5(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w6_p1 <- model_w6(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w7_p1 <- model_w7(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w8_p1 <- model_w8(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w9_p1 <- model_w9(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w10_p1 <- model_w10(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w11_p1 <- model_w11(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w12_p1 <- model_w12(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p1_preselection, min_train_p1, max_train_p1, max_test_p1)
model_w13_p1 <- model_w13(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p1_preselection, min_train_p1, max_train_p1, max_test_p1)

results_p1 = c(model_w1_p1, model_w2_p1, model_w3_p1, model_w4_p1, model_w5_p1, model_w6_p1, model_w7_p1, model_w8_p1, model_w9_p1, model_w10_p1, model_w11_p1, model_w12_p1, model_w13_p1)
results_p1 <- as.data.frame(results_p1)

rm(model_w1_p1, model_w2_p1, model_w3_p1, model_w4_p1, model_w5_p1, model_w6_p1, model_w7_p1, model_w8_p1, model_w9_p1, model_w10_p1, model_w11_p1, model_w12_p1, model_w13_p1)

# Period 2: Cyclical Stability -------------------------------------------------

min_train_p2 <- "2002-01-01"
max_train_p2 <- "2014-04-01"
max_test_p2 <- "2016-04-01"

p2_preselection <- preselection_results$p2
p2_preselection$keyword <- paste0(p2_preselection$keyword, "_b")
p2_preselection <- as.vector(p2_preselection[1])[[1]]

model_w1_p2 <- model_w1(gdp, attention_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w2_p2 <- model_w2(gdp, attention_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w3_p2 <- model_w3(gdp, attention_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w4_p2 <- model_w4(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w5_p2 <- model_w5(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w6_p2 <- model_w6(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w7_p2 <- model_w7(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w8_p2 <- model_w8(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w9_p2 <- model_w9(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w10_p2 <- model_w10(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w11_p2 <- model_w11(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w12_p2 <- model_w12(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p2_preselection, min_train_p2, max_train_p2, max_test_p2)
model_w13_p2 <- model_w13(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p2_preselection, min_train_p2, max_train_p2, max_test_p2)

results_p2 = c(model_w1_p2, model_w2_p2, model_w3_p2, model_w4_p2, model_w5_p2, model_w6_p2, model_w7_p2, model_w8_p2, model_w9_p2, model_w10_p2, model_w11_p2, model_w12_p2, model_w13_p2)
results_p2 <- as.data.frame(results_p2)

rm(model_w1_p2, model_w2_p2, model_w3_p2, model_w4_p2, model_w5_p2, model_w6_p2, model_w7_p2, model_w8_p2, model_w9_p2, model_w10_p2, model_w11_p2, model_w12_p2, model_w13_p2)

# Period 3: COVID pandemic -----------------------------------------------------

min_train_p3 <- "2002-01-01"
max_train_p3 <- "2019-04-01"
max_test_p3 <- "2021-04-01"

p3_preselection <- preselection_results$p3
p3_preselection$keyword <- paste0(p3_preselection$keyword, "_b")
p3_preselection <- as.vector(p3_preselection[1])[[1]]


model_w1_p3 <- model_w1(gdp, attention_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w2_p3 <- model_w2(gdp, attention_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w3_p3 <- model_w3(gdp, attention_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w4_p3 <- model_w4(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w5_p3 <- model_w5(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w6_p3 <- model_w6(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w7_p3 <- model_w7(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w8_p3 <- model_w8(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w9_p3 <- model_w9(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w10_p3 <- model_w10(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w11_p3 <- model_w11(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w12_p3 <- model_w12(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p3_preselection, min_train_p3, max_train_p3, max_test_p3)
model_w13_p3 <- model_w13(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p3_preselection, min_train_p3, max_train_p3, max_test_p3)

results_p3 = c(model_w1_p3, model_w2_p3, model_w3_p3, model_w4_p3, model_w5_p3, model_w6_p3, model_w7_p3, model_w8_p3, model_w9_p3, model_w10_p3, model_w11_p3, model_w12_p3, model_w13_p3)
results_p3 <- as.data.frame(results_p3)

rm(model_w1_p3, model_w2_p3, model_w3_p3, model_w4_p3, model_w5_p3, model_w6_p3, model_w7_p3, model_w8_p3, model_w9_p3, model_w10_p3, model_w11_p3, model_w12_p3, model_w13_p3)

# Period 4: Now ----------------------------------------------------------------

min_train_p4 <- "2002-01-01"
max_train_p4 <- "2022-04-01"
max_test_p4 <- "2024-04-01"

p4_preselection <- preselection_results$p4
p4_preselection$keyword <- paste0(p4_preselection$keyword, "_b")
p4_preselection <- as.vector(p4_preselection[1])[[1]]


model_w1_p4 <- model_w1(gdp, attention_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w2_p4 <- model_w2(gdp, attention_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w3_p4 <- model_w3(gdp, attention_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w4_p4 <- model_w4(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w5_p4 <- model_w5(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w6_p4 <- model_w6(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w7_p4 <- model_w7(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w8_p4 <- model_w8(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w9_p4 <- model_w9(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w10_p4 <- model_w10(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w11_p4 <- model_w11(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w12_p4 <- model_w12(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p4_preselection, min_train_p4, max_train_p4, max_test_p4)
model_w13_p4 <- model_w13(gdp, attention_bridge, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, p4_preselection, min_train_p4, max_train_p4, max_test_p4)

results_p4 = c(model_w1_p4, model_w2_p4, model_w3_p4, model_w4_p4, model_w5_p4, model_w6_p4, model_w7_p4, model_w8_p4, model_w9_p4, model_w10_p4, model_w11_p4, model_w12_p4, model_w13_p4)
results_p4 <- as.data.frame(results_p4)

rm(model_w1_p4, model_w2_p4, model_w3_p4, model_w4_p4, model_w5_p4, model_w6_p4, model_w7_p4, model_w8_p4, model_w9_p4, model_w10_p4, model_w11_p4, model_w12_p4, model_w13_p4)

# save results

save(results_p1, file = "/Users/lena/Documents/R/master_thesis/nowcasting results/results_p1_text005.Rda")
save(results_p2, file = "/Users/lena/Documents/R/master_thesis/nowcasting results/results_p2_text005.Rda")
save(results_p3, file = "/Users/lena/Documents/R/master_thesis/nowcasting results/results_p3_text005.Rda")
save(results_p4, file = "/Users/lena/Documents/R/master_thesis/nowcasting results/results_p4_text005.Rda")
