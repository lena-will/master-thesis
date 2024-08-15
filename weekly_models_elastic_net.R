# housekeeping -----------------------------------------------------------------

library(tidyverse)

# load text-based indicators ---------------------------------------------------

# load("/Users/lena/Documents/R/master_thesis/attention_issue.Rda")
# 
# z_scores <- function(x) {
#   (x - mean(x)) / sd(x)
# }
# 
# attention_isssue <- time_series_issue %>% 
#   group_by(topic) %>% 
#   mutate(attention_z = z_scores(attention_issue)) %>% 
#   ungroup() %>% 
#   select(topic, Datum, attention_z)
# 
# attention_wide <- attention_isssue %>% 
#   pivot_wider(names_from = topic, values_from = attention_z)
# 
# rm(time_series_issue, attention_isssue)
# 
# write.csv(attention_wide, "/Users/lena/Documents/R/master_thesis/data/text_indicators.csv")

# load macro data --------------------------------------------------------------

esi <- read.csv("/Users/lena/Documents/R/master_thesis/data/esi_mapping.csv") %>% 
  select(-X)
cpi <- read.csv("/Users/lena/Documents/R/master_thesis/data/cpi_mapping.csv") %>% 
  select(-X)
vacancies <- read.csv("/Users/lena/Documents/R/master_thesis/data/vacancies_mapping.csv")%>% 
  select(-X)
ip_index <- read.csv("/Users/lena/Documents/R/master_thesis/data/ip_index_mapping.csv")%>% 
  select(-X)
spread <- read.csv("/Users/lena/Documents/R/master_thesis/data/term_spread_mapping.csv")%>% 
  select(-X)
gdp_weekly <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv")%>% 
  select(-X)
text_indicator <- read.csv("/Users/lena/Documents/R/master_thesis/data/text_indicator_mapping.csv")%>% 
  select(-X) %>% 
  relocate(c(week_avail, quarter_avail), .before = 1)

# model week 12:

y <- gdp_weekly %>% 
  filter(quarter_week == 12)
esi_12 <- esi %>% 
  filter(week_avail == 12)
cpi_12 <- cpi %>% 
  filter(week_avail == 12)
vacancies_12 <- vacancies %>% 
  filter(week_avail == 12)
spread_12 <- spread %>% 
  filter(week_avail == 12)
ip_index_12 <- ip_index %>% 
  filter(week_avail == 12)



