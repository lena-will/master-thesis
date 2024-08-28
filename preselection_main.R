# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(zoo)
library(lubridate)

# Load text data and aggregate to quarterly frequency for preselection ---------

attention_ini <- read.csv("/Users/lena/Documents/R/master_thesis/final/issue_collapsed.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

attention_ini$month <- as.Date(sub("\\d{2}$", "1", attention_ini$Date))

attention <- attention_ini %>% 
  relocate(month, .after = Date) %>% 
  filter(Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  select(-X)

attention_preselection <- attention %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-c(document, Date, month, week_avail)) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean)

# Load macro data and aggregate to quarterly frequency for preselection --------

# esi

esi_preselection <- read.csv("/Users/lena/Documents/R/master_thesis/data/esi.csv") %>%
  select(-X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean) 

# cpi

cpi_preselection <- read.csv("/Users/lena/Documents/R/master_thesis/data/cpi.csv") %>%
  select(-c(X, CPI)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean)


# vacancies 

vacancies_preselection <- read.csv("/Users/lena/Documents/R/master_thesis/data/vacancies.csv") %>%
  select(-c(X, vacancies)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean)


# term spread

term_spread_preselection <- read.csv("/Users/lena/Documents/R/master_thesis/data/term_spread.csv") %>%
  select(-c(X, availability)) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean)


# ip index 

ip_index_preselection <- read.csv("/Users/lena/Documents/R/master_thesis/data/ip_index.csv") %>%
  select(-c(X, availability, IP)) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date) %>% 
  group_by(Quarter) %>% 
  summarise_all(mean)

# load dependent variable ------------------------------------------------------

gdp <- read.csv(file = "/Users/lena/Documents/R/master_thesis/data/gdp.csv") %>% 
  select(-GDP) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter (Date >= "2001-10-01" & Date < "2024-04-01") %>% 
  mutate(Quarter = format(as.yearqtr(Date, "%b-%Y"), "%YQ%q")) %>% 
  relocate(Quarter, .before = Date) %>% 
  select(-Date)

# ----------------------------- Preselection -----------------------------------

source("/Users/lena/Git/master-thesis/functions/preselection.R")

# Period 1: Recession

min_train_p1 = "2002Q1"
max_train_p1 = "2007Q2"

p1_preselection <- preselection(gdp, attention_preselection, esi_preselection, cpi_preselection, term_spread_preselection, vacancies_preselection, ip_index_preselection, min_train_p1, max_train_p1)
p1_selected <- p1_preselection %>% 
  filter(tau <= "0.1") %>% 
  select(keyword)
rownames(p1_selected) = c(seq(1,nrow(p1_selected),1))

# Period 2: Cyclical Stability

min_train_p2 = "2002Q1"
max_train_p2 = "2014Q2"

p2_preselection <- preselection(gdp, attention_preselection, esi_preselection, cpi_preselection, term_spread_preselection, vacancies_preselection, ip_index_preselection, min_train_p2, max_train_p2)
p2_selected <- p2_preselection %>% 
  filter(tau <= "0.1") %>% 
  select(keyword)
rownames(p2_selected) = c(seq(1,nrow(p2_selected),1))

# Period 3: Covid Pandemic

min_train_p3 = "2002Q1"
max_train_p3 = "2019Q2"

p3_preselection <- preselection(gdp, attention_preselection, esi_preselection, cpi_preselection, term_spread_preselection, vacancies_preselection, ip_index_preselection, min_train_p3, max_train_p3)
p3_selected <- p3_preselection %>% 
  filter(tau <= "0.1") %>% 
  select(keyword)
rownames(p3_selected) = c(seq(1,nrow(p3_selected),1))

# Period 4: Now

min_train_p4 = "2002Q1"
max_train_p4 = "2022Q2"

p4_preselection <- preselection(gdp, attention_preselection, esi_preselection, cpi_preselection, term_spread_preselection, vacancies_preselection, ip_index_preselection, min_train_p4, max_train_p4)
p4_selected <- p4_preselection %>% 
  filter(tau <= "0.1") %>% 
  select(keyword)
rownames(p4_selected) = c(seq(1,nrow(p4_selected),1))

# save preselected variables for nowcasting ------------------------------------

preselection_results <- list(p1 = p1_selected, p2 = p2_selected, p3 = p3_selected, p4 = p4_selected)
save(preselection_results, file = "/Users/lena/Documents/R/master_thesis/preselection_results.Rda")
