# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(zoo)

# load time series -------------------------------------------------------------

recessions <- read.csv("/Users/lena/Documents/R/master_thesis/recessions_germany.csv")

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp.csv")

# Period 1: Recession

gdp_recession <- gdp %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2006-01-01" & Date <= "2010-01-01")

plot_p1 <- gdp_recession %>% 
  ggplot(aes(x = Date, y = gdp_growth, group = 1)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%yQ%q"), breaks = "3 months") +
  theme_classic() +
  geom_hline(yintercept = 0.0, color = "grey") +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2008-01-01"), xmax = as.POSIXct("2009-04-01"),
           ymin = -Inf, ymax = Inf) +
  ylab("QoQ GDP Growth")

plot(plot_p1)

# Period 2: Cyclical Stability

gdp_p2 <- gdp %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2013-01-01" & Date <= "2017-01-01")

plot_p2 <- gdp_p2 %>% 
  ggplot(aes(x = Date, y = gdp_growth, group = 1)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%yQ%q"), breaks = "3 months") +
  theme_classic() +
  geom_hline(yintercept = 0.0, color = "grey") +
  ylab("QoQ GDP Growth")

plot(plot_p2)

# Period 3: COVID pandemic

gdp_p3 <- gdp %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2018-01-01" & Date <= "2022-01-01")

plot_p3 <- gdp_p3 %>% 
  ggplot(aes(x = Date, y = gdp_growth, group = 1)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%yQ%q"), breaks = "3 months") +
  theme_classic() +
  geom_hline(yintercept = 0.0, color = "grey") +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2019-10-01"), xmax = as.POSIXct("2020-04-01"),
           ymin = -Inf, ymax = Inf) +
  ylab("QoQ GDP Growth")

plot(plot_p3)

# Period 4: Now

gdp_p4 <- gdp %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2021-01-01" & Date <= "2024-04-01")

plot_p4 <- gdp_p4 %>% 
  ggplot(aes(x = Date, y = gdp_growth, group = 1)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%yQ%q"), breaks = "3 months") +
  theme_classic() +
  geom_hline(yintercept = 0.0, color = "grey") +
  ylab("QoQ GDP Growth")

plot(plot_p4)




