# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# load text-based indicators ---------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/attention_issue.Rda")

z_scores <- function(x) {
  (x - mean(x)) / sd(x)
}

attention_isssue <- time_series_issue %>% 
  group_by(topic) %>% 
  mutate(attention_z = z_scores(attention_issue)) %>% 
  ungroup() %>% 
  select(topic, Datum, attention_z)

attention_wide <- attention_isssue %>% 
  pivot_wider(names_from = topic, values_from = attention_z)

# load macro data --------------------------------------------------------------

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

macro_data <- read.csv("/Users/lena/Git/master-thesis/data/macro_data.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# format date for weekly models for all variables ------------------------------

esi <- macro_data %>% 
  select(Date, ESI)
esi$Date <- as.Date(sub("\\d{2}$", "28", esi$Date))

vacancies <- macro_data %>% 
  select(Date, vacancies)
vacancies$Date <- as.Date(sub("\\d{2}$", "28", vacancies$Date))

cpi <- macro_data %>% 
  select(Date, CPI)
cpi$Date <- as.Date(sub("\\d{2}$", "28", cpi$Date))

term_spread <- macro_data %>% 
  select(Date, spread) %>%
  mutate(availability = Date + days(45))

ip_index <- macro_data %>% 
  select(Date, IP) %>%
  mutate(availability = Date + days(75))

# prep gdp ---------------------------------------------------------------------

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp.csv")

gdp_weekly <- gdp %>% 
  select(c(Date, gdp_growth)) %>%
  slice(rep(1:nrow(gdp), each = 13)) %>% 
  mutate(quarter_week = rep(1:13, length.out = nrow(gdp_weekly)))

# save data --------------------------------------------------------------------

write.csv(esi, "/Users/lena/Documents/R/master_thesis/data/esi.csv")
write.csv(vacancies, "/Users/lena/Documents/R/master_thesis/data/vacancies.csv")
write.csv(cpi, "/Users/lena/Documents/R/master_thesis/data/cpi.csv")
write.csv(term_spread, "/Users/lena/Documents/R/master_thesis/data/term_spread.csv")
write.csv(ip_index, "/Users/lena/Documents/R/master_thesis/data/ip_index.csv")
write.csv(attention_wide, "/Users/lena/Documents/R/master_thesis/data/text_indicator.csv")
write.csv(gdp_weekly, "/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv")
