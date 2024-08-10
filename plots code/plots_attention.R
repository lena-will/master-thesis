# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(zoo)

# load time series -------------------------------------------------------------

load("/Users/lena/Documents/R/master_thesis/attention_month.Rda")
recessions <- read.csv("/Users/lena/Documents/R/master_thesis/recessions_germany.csv")

# some attention plots ---------------------------------------------------------

recessions <- recessions %>% 
  mutate(Peak = as.Date(Peak, format = "%Y-%m-%d")) %>% 
  mutate(Trough = as.Date(Trough, format = "%Y-%m-%d"))

monetary_economics <- time_series_month %>% 
  filter(topic == 46)

plot_topic_attention <- ggplot(monetary_economics) +
  geom_line(aes(x = month, y = attention, group = 1)) +
  scale_x_date(name = "Date", date_breaks = "1 year", date_labels="%Y", expand = c(0, 0)) +
  labs(x = "Date", y = "Monetary Policy") +
  theme_classic() +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3)

plot(plot_topic_attention)

