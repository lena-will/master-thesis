# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(zoo)

# load time series -------------------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/final/attention_issue_final.Rda")
recessions <- read.csv("/Users/lena/Documents/R/master_thesis/recessions_germany.csv")

recessions <- recessions %>% 
  mutate(Peak = as.Date(Peak, format = "%Y-%m-%d")) %>% 
  mutate(Trough = as.Date(Trough, format = "%Y-%m-%d"))

recessions[1,1] <- "2001-09-01" # alter peak for visualisation purposes

# aggregate to monthly attention -----------------------------------------------

attention_month <- attention_issue %>% 
  mutate(month = lubridate::floor_date(Date, 'month'), .after = Date) %>% 
  group_by(month) %>% 
  summarise(across(-(Date), mean))

# some attention plots ---------------------------------------------------------

# health care

health <- attention_month %>% 
  select(c(month, `38`)) %>% 
  rename(attention = `38`) %>% 
  rename(Date = month)

attention_health <- ggplot(health) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Health Care", breaks = seq(0.01, 0.08, 0.01)) +
  theme_classic(base_size = 14) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_health)

# financial crisis

financial_crisis <- attention_month %>% 
  select(c(month, `15`)) %>% 
  rename(attention = `15`) %>% 
  rename(Date = month)

attention_fin <- ggplot(financial_crisis) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Financial Crisis", breaks = seq(0.01, 0.08, 0.01)) +
  theme_classic(base_size = 14) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_fin)

# renewable energy

ren_energy <- attention_month %>% 
  select(c(month, `17`)) %>% 
  rename(attention = `17`) %>% 
  rename(Date = month)

attention_energy <- ggplot(ren_energy) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  labs(x = "Date", y = "Renewable Energy") +
  theme_classic(base_size = 14) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_energy)

# Government Budget

gov_budget <- attention_month %>% 
  select(c(month, `34`)) %>% 
  rename(attention = `34`) %>% 
  rename(Date = month)

attention_budget <- ggplot(gov_budget) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  labs(x = "Date", y = "Government Budget") +
  theme_classic(base_size = 14) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_budget)

# Negative sentiment 

neg <- attention_month %>% 
  select(c(month, `12`)) %>% 
  rename(attention = `12`) %>% 
  rename(Date = month)

attention_negative <- ggplot(neg) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Negative Sentiment", breaks = seq(0.01, 0.03, 0.005)) +
  theme_classic(base_size = 14) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_negative)

# Elections

elections <- attention_month %>% 
  select(c(month, `18`)) %>% 
  rename(attention = `18`) %>% 
  rename(Date = month)

attention_elections <- ggplot(elections) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "2 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Elections", breaks = seq(0.01, 0.03, 0.005)) +
  theme_classic() +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_elections)

# example topic plot

example <- attention_month %>% 
  select(c(month, `23`)) %>% 
  rename(attention = `23`) %>% 
  rename(Date = month)

attention_example <- ggplot(example) +
  geom_line(aes(x = Date, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "1 year", date_labels="%Y", expand = c(0, 0)) +
  scale_y_continuous(name = "Society", breaks = seq(0.00, 0.025, 0.005)) +
  theme_classic() +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(attention_example)

